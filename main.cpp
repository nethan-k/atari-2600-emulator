#include <iostream>
#include "components/6502.h"
#include "components/bus.h"

int main() {

	CPU6502 cpu;
	std::cout << "reached here" << std::endl;

	cpu.bus = new Bus;

	/* initialize rom w/ reset code */
	cpu.bus->rom[0x00] = 0x78;	/* sei */
	cpu.bus->rom[0x01] = 0xd8;	/* cld */
	cpu.bus->rom[0x02] = 0xa2;	/* ldx */
	cpu.bus->rom[0x03] = 0x00;	/* #$0 */
	cpu.bus->rom[0x04] = 0x8a;	/* txa */
	cpu.bus->rom[0x05] = 0xa8;	/* tay */
	cpu.bus->rom[0x06] = 0xca;	/* dex */
	cpu.bus->rom[0x07] = 0x9a;	/* txs */
	cpu.bus->rom[0x08] = 0x48;	/* pha */
	cpu.bus->rom[0x09] = 0xd0;	/* bne */
	cpu.bus->rom[0x0a] = 0x06;	/* #$6 */

	/* calling cpu functions directly */
	/* cpu.am_imp();
	cpu.op_sei();

	cpu.am_imp();
	cpu.op_cld();

	cpu.am_imm();
	cpu.op_ldx();

	cpu.am_imp();
	cpu.op_txa();
		
	cpu.am_imp();
	cpu.op_tay();

	for(int i = 0xFF; i >= 0x00; i--) {
		cpu.am_imp();
		cpu.op_dex();

		cpu.am_imp();
		cpu.op_txs();

		cpu.am_imp();
		cpu.op_pha();
	} */

	delete cpu.bus;
	return 0;
}
