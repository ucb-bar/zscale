led_c_src = \
	led.c \
	trap.c \

led_riscv_src = \
	crt.S \

led_c_objs     = $(patsubst %.c, %.o, $(led_c_src))
led_riscv_objs = $(patsubst %.S, %.o, $(led_riscv_src))

led_riscv_bin = led.riscv
$(led_riscv_bin) : $(led_c_objs) $(led_riscv_objs)
	$(RISCV_LINK) $(led_c_objs) $(led_riscv_objs) -o $(led_riscv_bin) $(RISCV_LINK_OPTS)

junk += $(led_c_objs) $(led_riscv_objs) $(led_host_bin) $(led_riscv_bin)
