#include "config.h"

#include <memory>
#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/SelfExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"

namespace Emacs
{

using llvm::cantFail;
using llvm::DataLayout;
using llvm::Error;
using llvm::Expected;
using llvm::IntrusiveRefCntPtr;
using llvm::MemoryBuffer;
using llvm::SectionMemoryManager;
using llvm::StringRef;

using llvm::orc::ConcurrentIRCompiler;
using llvm::orc::DynamicLibrarySearchGenerator;
using llvm::orc::ExecutionSession;
using llvm::orc::ExecutorSymbolDef;
using llvm::orc::IRCompileLayer;
using llvm::orc::JITDylib;
using llvm::orc::JITTargetMachineBuilder;
using llvm::orc::MangleAndInterner;
using llvm::orc::ResourceTrackerSP;
using llvm::orc::RTDyldObjectLinkingLayer;
using llvm::orc::SelfExecutorProcessControl;
using llvm::orc::ThreadSafeModule;

/* This is from the LLVM Kaleidoscope example.
   llvm-project/llvm/examples/Kaleidoscope/BuildingAJIT/Chapter1/ */

class JIT
{
private:
  std::unique_ptr<ExecutionSession> ES;

  DataLayout DL;
  MangleAndInterner Mangle;

  RTDyldObjectLinkingLayer ObjectLayer;
  IRCompileLayer CompileLayer;

  JITDylib &MainJD;

public:
  JIT (std::unique_ptr<ExecutionSession> ES,
       JITTargetMachineBuilder JTMB, DataLayout DL)
      : ES (std::move (ES)), DL (std::move (DL)),
	Mangle (*this->ES, this->DL),
	ObjectLayer (*this->ES,
		     [] (const MemoryBuffer &)
		       {
			 return std::make_unique<
			   SectionMemoryManager> ();
		       }),
	CompileLayer (*this->ES, ObjectLayer,
		      std::make_unique<ConcurrentIRCompiler> (
			std::move (JTMB))),
	MainJD (this->ES->createBareJITDylib ("<main>"))
  {
    MainJD.addGenerator (
      cantFail (DynamicLibrarySearchGenerator::GetForCurrentProcess (
	DL.getGlobalPrefix ())));
  }

  ~JIT ()
  {
    if (auto Err = ES->endSession ())
      ES->reportError (std::move (Err));
  }

  static Expected<std::unique_ptr<JIT>> Create ()
  {
    auto EPC = SelfExecutorProcessControl::Create ();
    if (!EPC)
      return EPC.takeError ();

    auto ES = std::make_unique<ExecutionSession> (std::move (*EPC));

    JITTargetMachineBuilder JTMB (
      ES->getExecutorProcessControl ().getTargetTriple ());

    auto DL = JTMB.getDefaultDataLayoutForTarget ();
    if (!DL)
      return DL.takeError ();

    return std::make_unique<JIT> (std::move (ES), std::move (JTMB),
				  std::move (*DL));
  }

  const DataLayout &getDataLayout () const { return DL; }

  JITDylib &getMainJITDylib () { return MainJD; }

  Error addModule (ThreadSafeModule TSM,
		   ResourceTrackerSP RT = nullptr)
  {
    if (!RT)
      RT = MainJD.getDefaultResourceTracker ();
    return CompileLayer.add (RT, std::move (TSM));
  }

  Expected<ExecutorSymbolDef> lookup (StringRef Name)
  {
    return ES->lookup ({ &MainJD }, Mangle (Name.str ()));
  }
};

#if 0
int
compile_c (const char *code)
{
  const char code[]
    = "extern void libc_puts(const char*);"
      "struct S { int a; int b; };"
      "static void init_a(struct S* s) { s->a = 1111; }"
      "static void init_b(struct S* s) { s->b = 2222; }"
      "void init(struct S* s) {"
      "init_a(s); init_b(s);"
      "libc_puts(\"libc_puts()\"); }";

  auto R = emacs::CCompiler ().compile (code);
  // Abort if compilation failed.
  auto [C, M] = cantFail (std::move (R));
  // M->print(llvm::errs(), nullptr);

  // -- JIT compiler the IR module.

  llvm::InitializeNativeTarget ();
  llvm::InitializeNativeTargetAsmPrinter ();

  auto JIT = emacs::Jit::Create ();
  auto TSM
    = llvm::orc::ThreadSafeModule (std::move (M), std::move (C));

  auto RT = JIT->addModule (std::move (TSM));
  if (auto E = RT.takeError ())
    {
      llvm::errs () << llvm::toString (std::move (E)) << '\n';
      return 1;
    }

  if (auto ADDR = JIT->lookup ("init"))
    {
      std::printf ("JIT ADDR 0x%lx\n",
		   (*ADDR).getAddress ().getValue ());

      struct S
      {
	int a, b;
      } state = { 0, 0 };
      auto JIT_FN = (*ADDR).getAddress ().toPtr<void (struct S *)> ();

      std::printf ("S { a=%d b=%d }\n", state.a, state.b);
      JIT_FN (&state);
      std::printf ("S { a=%d b=%d }\n", state.a, state.b);
    }

  // Remove jitted code tracked by this RT.
  cantFail ((*RT)->remove ());

  if (auto E = JIT->lookup ("init").takeError ())
    {
      // In ERROR state, as expected, consume the error.
      llvm::consumeError (std::move (E));
    }
  else
    {
      // In SUCCESS state, not expected as code was dropped.
      llvm::errs ()
	<< "Expected error, we removed code tracked by RT and "
	   "hence 'init' should be "
	   "removed from the JIT!\n";
    }

  return 0;
}


extern "C"
{
  bool llvm_compile (const char *c)
  {
    return emacs::compile_c (c) == 0;
  }
}
#endif

} // namespace Emacs
