/* Emacs DTrace provider. */

provider emacs {
    probe test_probe(const char *);
};

/* See https://docs.oracle.com/cd/E19253-01/817-6223/chp-usdt/index.html */
#pragma D attributes Evolving/Evolving/Common provider emacs provider
#pragma D attributes Evolving/Evolving/Common provider emacs module
#pragma D attributes Evolving/Evolving/Common provider emacs function
#pragma D attributes Evolving/Evolving/Common provider emacs name
#pragma D attributes Evolving/Evolving/Common provider emacs args
