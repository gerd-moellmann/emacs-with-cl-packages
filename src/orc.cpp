
#include <clang/Basic/DiagnosticOptions.h>
#include <clang/CodeGen/CodeGenAction.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/Lex/PreprocessorOptions.h>

#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/Shared/ExecutorAddress.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/TargetParser/Host.h>

namespace emacs
{

using clang::CompilerInstance;
using clang::CompilerInvocation;
using clang::DiagnosticConsumer;
using clang::DiagnosticOptions;
using clang::DiagnosticsEngine;
using clang::EmitLLVMOnlyAction;
using clang::TextDiagnosticPrinter;

using llvm::Expected;
using llvm::IntrusiveRefCntPtr;
using llvm::LLVMContext;
using llvm::MemoryBuffer;
using llvm::Module;
using llvm::StringError;

class CCompiler
{
public:
  CCompiler ()
  {
    // Setup custom diagnostic options.
    auto DO = IntrusiveRefCntPtr<DiagnosticOptions> (
      new DiagnosticOptions ());
    DO->ShowColors = 1;

    // Setup stderr custom diagnostic consumer.
    DC = std::make_unique<TextDiagnosticPrinter> (llvm::errs (),
						  DO.get ());

    // Create custom diagnostics engine.
    // The engine will NOT take ownership of the DiagnosticConsumer
    // object.
    DE = std::make_unique<
      DiagnosticsEngine> (nullptr /* DiagnosticIDs */, std::move (DO),
			  DC.get (),
			  false /* own DiagnosticConsumer */);
  }

  struct CompileResult
  {
    std::unique_ptr<LLVMContext> C;
    std::unique_ptr<Module> M;
  };

  Expected<CompileResult> compile (const char *code) const
  {
    using std::errc;
    const auto err
      = [] (errc ec) { return std::make_error_code (ec); };

    const char code_fname[] = "jit.c";

    // Create compiler instance.
    CompilerInstance CC;

    // Setup compiler invocation.
    bool ok
      = CompilerInvocation::CreateFromArgs (CC.getInvocation (),
					    { code_fname }, *DE);
    // We control the arguments, so we assert.
    assert (ok);

    // Setup custom diagnostic printer.
    CC.createDiagnostics (DC.get (),
			  false /* own DiagnosticConsumer */);

    // Configure remapping from pseudo file name to in-memory code
    // buffer code_fname -> code_buffer.
    //
    // PreprocessorOptions take ownership of MemoryBuffer.
    CC.getPreprocessorOpts ()
      .addRemappedFile (code_fname,
			MemoryBuffer::getMemBuffer (code).release ());

    // Configure codegen options.
    auto &CG = CC.getCodeGenOpts ();
    CG.OptimizationLevel = 3;
    CG.setInlining (clang::CodeGenOptions::NormalInlining);

    // Generate LLVM IR.
    EmitLLVMOnlyAction A;
    if (!CC.ExecuteAction (A))
      {
	return llvm::make_error<
	  StringError> ("Failed to generate LLVM IR from C code!",
			err (errc::invalid_argument));
      }

    // Take generated LLVM IR module and the LLVMContext.
    auto M = A.takeModule ();
    auto C = std::unique_ptr<LLVMContext> (A.takeLLVMContext ());

    // TODO: Can this become nullptr when the action succeeds?
    assert (M);

    return CompileResult{ std::move (C), std::move (M) };
  }

private:
  std::unique_ptr<DiagnosticConsumer> DC;
  std::unique_ptr<DiagnosticsEngine> DE;
};

using llvm::cantFail;
using llvm::DataLayout;
using llvm::Expected;
using llvm::JITEvaluatedSymbol;
using llvm::JITSymbolFlags;
using llvm::SectionMemoryManager;
using llvm::StringRef;

using llvm::orc::ConcurrentIRCompiler;
// using llvm::orc::DynamicLibrarySearchGenerator;
using llvm::orc::ExecutionSession;
using llvm::orc::ExecutorAddr;
using llvm::orc::ExecutorSymbolDef;
using llvm::orc::IRCompileLayer;
using llvm::orc::JITDylib;
using llvm::orc::JITTargetMachineBuilder;
using llvm::orc::MangleAndInterner;
using llvm::orc::ResourceTrackerSP;
using llvm::orc::RTDyldObjectLinkingLayer;
using llvm::orc::SelfExecutorProcessControl;
using llvm::orc::ThreadSafeModule;

// Simple JIT engine based on the KaleidoscopeJIT.
// https://www.llvm.org/docs/tutorial/BuildingAJIT1.html
class Jit
{
private:
  std::unique_ptr<ExecutionSession> ES;

  DataLayout DL;
  MangleAndInterner Mangle;

  RTDyldObjectLinkingLayer ObjectLayer;
  IRCompileLayer CompileLayer;

  JITDylib &JD;

public:
  Jit (std::unique_ptr<ExecutionSession> ES,
       JITTargetMachineBuilder JTMB, DataLayout DL)
      : ES (std::move (ES)), DL (std::move (DL)),
	Mangle (*this->ES, this->DL),
	ObjectLayer (*this->ES,
		     [] ()
		       {
			 return std::make_unique<
			   SectionMemoryManager> ();
		       }),
	CompileLayer (*this->ES, ObjectLayer,
		      std::make_unique<ConcurrentIRCompiler> (
			std::move (JTMB))),
	JD (this->ES->createBareJITDylib ("main"))
  {
    // https://www.llvm.org/docs/ORCv2.html#how-to-add-process-and-library-symbols-to-jitdylibs
    // JD.addGenerator(
    //     cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
    //         DL.getGlobalPrefix())));
    cantFail (JD.define (llvm::orc::absoluteSymbols (
      { { Mangle ("libc_puts"),
	  { ExecutorAddr::fromPtr (&puts),
	    JITSymbolFlags::Exported } } })));
  }

  ~Jit ()
  {
    if (auto Err = ES->endSession ())
      ES->reportError (std::move (Err));
  }

  static std::unique_ptr<Jit> Create ()
  {
    auto EPC = cantFail (SelfExecutorProcessControl::Create ());
    auto ES = std::make_unique<ExecutionSession> (std::move (EPC));

    JITTargetMachineBuilder JTMB (
      ES->getExecutorProcessControl ().getTargetTriple ());

    auto DL = cantFail (JTMB.getDefaultDataLayoutForTarget ());

    return std::make_unique<Jit> (std::move (ES), std::move (JTMB),
				  std::move (DL));
  }

  Expected<ResourceTrackerSP> addModule (ThreadSafeModule TSM)
  {
    auto RT = JD.createResourceTracker ();
    if (auto E = CompileLayer.add (RT, std::move (TSM)))
      {
	return E;
      }
    return RT;
  }

  Expected<ExecutorSymbolDef> lookup (StringRef Name)
  {
    return ES->lookup ({ &JD }, Mangle (Name.str ()));
  }
};

} // namespace emacs

int
llvm_compile (const char *code)
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
