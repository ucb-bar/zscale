mbist_c_src = \
	mbist.c \
	trap.c \

mbist_riscv_src = \
	crt.S \

mbist_c_objs     = $(patsubst %.c, %.o, $(mbist_c_src))
mbist_riscv_objs = $(patsubst %.S, %.o, $(mbist_riscv_src))

mbist_riscv_bin = mbist.riscv
$(mbist_riscv_bin) : $(mbist_c_objs) $(mbist_riscv_objs)
	$(RISCV_LINK) $(mbist_c_objs) $(mbist_riscv_objs) -o $(mbist_riscv_bin) $(RISCV_LINK_OPTS)

junk += $(mbist_c_objs) $(mbist_riscv_objs) $(mbist_host_bin) $(mbist_riscv_bin)
