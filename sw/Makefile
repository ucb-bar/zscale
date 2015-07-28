default: all

progdir = .

#--------------------------------------------------------------------
# Sources
#--------------------------------------------------------------------

progs = \
	led \
	mbist \

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

RISCV_PREFIX=riscv64-unknown-elf-
RISCV_GCC = $(RISCV_PREFIX)gcc
RISCV_GCC_OPTS = -m32 -static -Wa,-march=RVIM -std=gnu99 -O2 -fno-common -fno-builtin-printf
RISCV_LINK = $(RISCV_GCC) -m32 -T $(progdir)/common/test.ld
RISCV_LINK_OPTS = -nostdlib -nostartfiles -ffast-math -lc -lgcc
RISCV_OBJDUMP = $(RISCV_PREFIX)objdump --disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.data

VPATH += $(addprefix $(progdir)/, $(progs))
VPATH += $(progdir)/common

incs  += -I$(progdir)/common $(addprefix -I$(progdir)/, $(progs))

include $(patsubst %, $(progdir)/%/prog.mk, $(progs))

#------------------------------------------------------------
# Build

progs_riscv_bin  = $(addsuffix .riscv, $(progs))
progs_riscv_dump = $(addsuffix .riscv.dump, $(progs))
progs_riscv_hex  = $(addsuffix .riscv.hex, $(progs))

%.hex: %
	elf2hex 16 8192 $< > $@

$(progs_riscv_dump): %.riscv.dump: %.riscv
	$(RISCV_OBJDUMP) $< > $@

%.o: %.c
	$(RISCV_GCC) $(RISCV_GCC_OPTS) -c $(incs) $< -o $@

%.o: %.S
	$(RISCV_GCC) $(RISCV_GCC_OPTS) -D__ASSEMBLY__=1 -c $(incs) $< -o $@

riscv: $(progs_riscv_dump) $(progs_riscv_hex)

junk += $(progs_riscv_bin) $(progs_riscv_dump) $(progs_riscv_hex)

#------------------------------------------------------------
# Default

all: riscv

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(junk)
