# Mercury
MMC := mmc

srcdir := ../src
srcs := *.m \
	$(srcdir)/*.m
m_files := $(wildcard $(srcs))

PROGS = httpsrv_test

.PHONY: default
default: $(PROGS)

.PHONY: $(PROGS)
$(PROGS): $(m_files) Mercury.modules Mercury.options Mercury.params
	@$(MMC) --make $@ && touch $@

Mercury.modules: $(m_files) Makefile
	@echo "Making Mercury.modules ($(words $(m_files)) files)"
	@$(MMC) -f $(m_files)

Mercury.params:

tags: $(m_files)
	@echo "Making tags ($(words $(m_files)) files)"
	@mtags $(m_files)
