#include "6502.h"
#include "bus.h"

#include <iostream>

CPU6502::CPU6502() {

	/* lookup table with instruction name, operation, addressing mode, no. of clock cycles */
	using a = CPU6502;
	lookup = {
		{ "BRK", &a::op_brk, &a::am_imm, 7 },
		{ "ORA", &a::op_ora, &a::am_izx, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "???", &a::op_nop, &a::am_imp, 3 },
		{ "ORA", &a::op_ora, &a::am_zpg, 3 },
		{ "ASL", &a::op_asl, &a::am_zpg, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 5 },
		{ "PHP", &a::op_php, &a::am_imp, 3 },
		{ "ORA", &a::op_ora, &a::am_imm, 2 },
		{ "ASL", &a::op_asl, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "ORA", &a::op_ora, &a::am_abs, 4 },
		{ "ASL", &a::op_asl, &a::am_abs, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },

		{ "BPL", &a::op_bpl, &a::am_rel, 2 },
		{ "ORA", &a::op_ora, &a::am_izy, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "ORA", &a::op_ora, &a::am_zpx, 4 },
		{ "ASL", &a::op_asl, &a::am_zpx, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },
		{ "CLC", &a::op_clc, &a::am_imp, 2 },
		{ "ORA", &a::op_ora, &a::am_aby, 4 },
		{ "???", &a::op_nop, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 7 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "ORA", &a::op_ora, &a::am_abx, 4 },
		{ "ASL", &a::op_asl, &a::am_abx, 7 },
		{ "???", &a::op_xxx, &a::am_imp, 7 },

		{ "JSR", &a::op_jsr, &a::am_abs, 6 },
		{ "AND", &a::op_and, &a::am_izx, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "BIT", &a::op_bit, &a::am_zpg, 3 },
		{ "AND", &a::op_and, &a::am_zpg, 3 },
		{ "ROL", &a::op_rol, &a::am_zpg, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 5 },
		{ "PLP", &a::op_plp, &a::am_imp, 4 },
		{ "AND", &a::op_and, &a::am_imm, 2 },
		{ "ROL", &a::op_rol, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "BIT", &a::op_bit, &a::am_abs, 4 },
		{ "AND", &a::op_and, &a::am_abs, 4 },
		{ "ROL", &a::op_rol, &a::am_abs, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },

		{ "BMI", &a::op_bmi, &a::am_rel, 2 },
		{ "AND", &a::op_and, &a::am_izy, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "AND", &a::op_and, &a::am_zpx, 4 },
		{ "ROL", &a::op_rol, &a::am_zpx, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },
		{ "SEC", &a::op_sec, &a::am_imp, 2 },
		{ "AND", &a::op_and, &a::am_aby, 4 },
		{ "???", &a::op_nop, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 7 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "AND", &a::op_and, &a::am_abx, 4 },
		{ "ROL", &a::op_rol, &a::am_abx, 7 },
		{ "???", &a::op_xxx, &a::am_imp, 7 },

		{ "RTI", &a::op_rti, &a::am_imp, 6 },
		{ "EOR", &a::op_eor, &a::am_izx, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "???", &a::op_nop, &a::am_imp, 3 },
		{ "EOR", &a::op_eor, &a::am_zpg, 3 },
		{ "LSR", &a::op_lsr, &a::am_zpg, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 5 },
		{ "PHA", &a::op_pha, &a::am_imp, 3 },
		{ "EOR", &a::op_eor, &a::am_imm, 2 },
		{ "LSR", &a::op_lsr, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "JMP", &a::op_jmp, &a::am_abs, 3 },
		{ "EOR", &a::op_eor, &a::am_abs, 4 },
		{ "LSR", &a::op_lsr, &a::am_abs, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },

		{ "BVC", &a::op_bvc, &a::am_rel, 2 },
		{ "EOR", &a::op_eor, &a::am_izy, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "EOR", &a::op_eor, &a::am_zpg, 4 },
		{ "LSR", &a::op_lsr, &a::am_zpx, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },
		{ "CLI", &a::op_cli, &a::am_imp, 2 },
		{ "EOR", &a::op_eor, &a::am_aby, 4 },
		{ "???", &a::op_nop, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 7 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "EOR", &a::op_eor, &a::am_abx, 4 },
		{ "LSR", &a::op_lsr, &a::am_abx, 7 },

		{ "???", &a::op_xxx, &a::am_imp, 7 },
		{ "RTS", &a::op_rts, &a::am_imp, 6 },
		{ "ADC", &a::op_adc, &a::am_izx, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "???", &a::op_nop, &a::am_imp, 3 },
		{ "ADC", &a::op_adc, &a::am_zpg, 3 },
		{ "ROR", &a::op_ror, &a::am_zpg, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 5 },
		{ "PLA", &a::op_pla, &a::am_imp, 4 },
		{ "ADC", &a::op_adc, &a::am_imm, 2 },
		{ "ROR", &a::op_ror, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "JMP", &a::op_jmp, &a::am_ind, 5 },
		{ "ADC", &a::op_adc, &a::am_abs, 4 },
		{ "ROR", &a::op_ror, &a::am_abs, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },

		{ "BVS", &a::op_bvs, &a::am_rel, 2 },
		{ "ADC", &a::op_adc, &a::am_izy, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "ADC", &a::op_adc, &a::am_zpx, 4 },
		{ "ROR", &a::op_ror, &a::am_zpx, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },
		{ "SEI", &a::op_sei, &a::am_imp, 2 },
		{ "ADC", &a::op_adc, &a::am_aby, 4 },
		{ "???", &a::op_nop, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 7 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "ADC", &a::op_adc, &a::am_abx, 4 },
		{ "ROR", &a::op_ror, &a::am_abx, 7 },
		{ "???", &a::op_xxx, &a::am_imp, 7 },

		{ "???", &a::op_nop, &a::am_imp, 2 },
		{ "STA", &a::op_sta, &a::am_izx, 6 },
		{ "???", &a::op_nop, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },
		{ "STY", &a::op_sty, &a::am_zpg, 3 },
		{ "STA", &a::op_sta, &a::am_zpg, 3 },
		{ "STX", &a::op_stx, &a::am_zpg, 3 },
		{ "???", &a::op_xxx, &a::am_imp, 3 },
		{ "DEY", &a::op_dey, &a::am_imp, 2 },
		{ "???", &a::op_nop, &a::am_imp, 2 },
		{ "TXA", &a::op_txa, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "STY", &a::op_sty, &a::am_abs, 4 },
		{ "STA", &a::op_sta, &a::am_abs, 4 },
		{ "STX", &a::op_stx, &a::am_abs, 4 },
		{ "???", &a::op_xxx, &a::am_imp, 4 },

		{ "BCC", &a::op_bcc, &a::am_rel, 2 },
		{ "STA", &a::op_sta, &a::am_izy, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },
		{ "STY", &a::op_sty, &a::am_zpx, 4 },
		{ "STA", &a::op_sta, &a::am_zpx, 4 },
		{ "STX", &a::op_stx, &a::am_zpy, 4 },
		{ "???", &a::op_xxx, &a::am_imp, 4 },
		{ "TYA", &a::op_tya, &a::am_imp, 2 },
		{ "STA", &a::op_sta, &a::am_aby, 5 },
		{ "TXS", &a::op_txs, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 5 },
		{ "???", &a::op_nop, &a::am_imp, 5 },
		{ "STA", &a::op_sta, &a::am_abx, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 5 },

		{ "LDY", &a::op_ldy, &a::am_imm, 2 },
		{ "LDA", &a::op_lda, &a::am_izx, 6 },
		{ "LDX", &a::op_ldx, &a::am_imm, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },
		{ "LDY", &a::op_ldy, &a::am_zpg, 3 },
		{ "LDA", &a::op_lda, &a::am_zpg, 3 },
		{ "LDX", &a::op_ldx, &a::am_zpg, 3 },
		{ "???", &a::op_xxx, &a::am_imp, 3 },
		{ "TAY", &a::op_tay, &a::am_imp, 2 },
		{ "LDA", &a::op_lda, &a::am_imm, 2 },
		{ "TAX", &a::op_tax, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "LDY", &a::op_ldy, &a::am_abs, 4 },
		{ "LDA", &a::op_lda, &a::am_abs, 4 },
		{ "LDX", &a::op_ldx, &a::am_abs, 4 },
		{ "???", &a::op_xxx, &a::am_imp, 4 },

		{ "BCS", &a::op_bcs, &a::am_rel, 2 },
		{ "LDA", &a::op_lda, &a::am_izy, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 5 },
		{ "LDY", &a::op_ldy, &a::am_zpx, 4 },
		{ "LDA", &a::op_lda, &a::am_zpx, 4 },
		{ "LDX", &a::op_ldx, &a::am_zpy, 4 },
		{ "???", &a::op_xxx, &a::am_imp, 4 },
		{ "CLV", &a::op_clv, &a::am_imp, 2 },
		{ "LDA", &a::op_lda, &a::am_aby, 4 },
		{ "TSX", &a::op_tsx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 4 },
		{ "LDY", &a::op_ldy, &a::am_abx, 4 },
		{ "LDA", &a::op_lda, &a::am_abx, 4 },
		{ "LDX", &a::op_ldx, &a::am_aby, 4 },
		{ "???", &a::op_xxx, &a::am_imp, 4 },

		{ "CPY", &a::op_cpy, &a::am_imm, 2 },
		{ "CMP", &a::op_cmp, &a::am_izx, 6 },
		{ "???", &a::op_nop, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "CPY", &a::op_cpy, &a::am_zpg, 3 },
		{ "CMP", &a::op_cmp, &a::am_zpg, 3 },
		{ "DEC", &a::op_dec, &a::am_zpg, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 5 },
		{ "INY", &a::op_iny, &a::am_imp, 2 },
		{ "CMP", &a::op_cmp, &a::am_imm, 2 },
		{ "DEX", &a::op_dex, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "CPY", &a::op_cpy, &a::am_abs, 4 },
		{ "CMP", &a::op_cmp, &a::am_abs, 4 },
		{ "DEC", &a::op_dec, &a::am_abs, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },

		{ "BNE", &a::op_bne, &a::am_rel, 2 },
		{ "CMP", &a::op_cmp, &a::am_izy, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "CMP", &a::op_cmp, &a::am_zpx, 4 },
		{ "DEC", &a::op_dec, &a::am_zpx, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },
		{ "CLD", &a::op_cld, &a::am_imp, 2 },
		{ "CMP", &a::op_cmp, &a::am_aby, 4 },
		{ "NOP", &a::op_nop, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 7 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "CMP", &a::op_cmp, &a::am_abx, 4 },
		{ "DEC", &a::op_dec, &a::am_abx, 7 },
		{ "???", &a::op_xxx, &a::am_imp, 7 },

		{ "CPX", &a::op_cpx, &a::am_imm, 2 },
		{ "SBC", &a::op_sbc, &a::am_izx, 6 },
		{ "???", &a::op_nop, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "CPX", &a::op_cpx, &a::am_zpg, 3 },
		{ "SBC", &a::op_sbc, &a::am_zpg, 3 },
		{ "INC", &a::op_inc, &a::am_zpg, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 5 },
		{ "INX", &a::op_inx, &a::am_imp, 2 },
		{ "SBC", &a::op_sbc, &a::am_imm, 2 },
		{ "NOP", &a::op_nop, &a::am_imp, 2 },
		{ "???", &a::op_sbc, &a::am_imp, 2 },
		{ "CPX", &a::op_cpx, &a::am_abs, 4 },
		{ "SBC", &a::op_sbc, &a::am_abs, 4 },
		{ "INC", &a::op_inc, &a::am_abs, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },

		{ "BEQ", &a::op_beq, &a::am_rel, 2 },
		{ "SBC", &a::op_sbc, &a::am_izy, 5 },
		{ "???", &a::op_xxx, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 8 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "SBC", &a::op_sbc, &a::am_zpx, 4 },
		{ "INC", &a::op_inc, &a::am_zpx, 6 },
		{ "???", &a::op_xxx, &a::am_imp, 6 },
		{ "SED", &a::op_sed, &a::am_imp, 2 },
		{ "SBC", &a::op_sbc, &a::am_aby, 4 },
		{ "NOP", &a::op_nop, &a::am_imp, 2 },
		{ "???", &a::op_xxx, &a::am_imp, 7 },
		{ "???", &a::op_nop, &a::am_imp, 4 },
		{ "SBC", &a::op_sbc, &a::am_abx, 4 },
		{ "INC", &a::op_inc, &a::am_abx, 7 },
		{ "???", &a::op_xxx, &a::am_imp, 7 }
	};
}

CPU6502::~CPU6502() {

}

/*
 * hardware signals
 * ~~~~~~~~~~~~~~~~
*/

/* reset signal */
void reset() {

	/* set program counter */
	uint8_t hi = bus->read(0xfffc);
	uint8_t lo = bus->read(0xfffd);
	pc = (hi << 8) | lo;

	a = 0x00;
	x = 0x00;
	y = 0x00;
	sp = 0x00;
	sr = 0x00 | U; /* U is always set */

	fetched = 0x00;
	addr = 0x00;
	opcode = 0x00;
	cycles = 0;
}

/* interrupt request */
void irq() {
	if(get_flag(I) == 0) {
		
		/* push program counter to stack */
		bus->write(0x0100+sp--, (pc >> 8) & 0x00ff);
		bus->write(0x0100+sp--, pc & 0x00ff);

		/* push status register */
		set_flag(B, 0); /* if B is pushed by nmi or irq, B = 0. if b is pushed by brk or php, B = 1 */
		set_flag(U, 1); /* U is always set */
		set_flag(I, 1);
		bus->write(0x0100+sp--, sr);

		/* read new program counter */
		uint8_t hi = bus->read(0xfffe);
		uint8_t lo = bus->read(0xffff);
		pc = (hi << 8) | lo;

		cycles = 7;
	}
}

/* non-maskable interrupt */
void nmi() {
	
	/* push program counter to stack */
	bus->write(0x0100+sp--, (pc >> 8) & 0x00ff);
	bus->write(0x0100+sp--, pc & 0x00ff);

	/* push status register */
	set_flag(B, 0);
	set_flag(U, 1);
	set_flag(I, 1);
	bus->write(0x0100+sp--, sr);

	/* read new program counter */
	uint8_t hi = bus->read(0xfffa);
	uint8_t lo = bus->read(0xfffb);
	pc = (hi << 8) | lo;

	cycles = 8;
}

/* clock signal */
void clock() {
	if(cycles == 0) {
		set_flag(U, true); /* always set */

		opcode = bus->read(pc++);
		cycles = lookup[opcode].cycles;
		
		uint8_t am_cycle = (this->*lookup[opcode].am)();
		fetched = fetch();
		uint8_t op_cycle = (this->*lookup[opcode].op)();

		/* additional cycle required if both addressing mode & operating require it */
		cycles += (am_cycle & op_cycle);
	}

	cycles--;
}

/* utility functions */

uint8_t CPU6502::get_flag(CPU6502::FLAGS f) {
	return ((f & 0) > 0) ? 1 : 0;
}

void CPU6502::set_flag(CPU6502::FLAGS f, bool v) {
	if(v > 0) {
		sr |= 1;
		return;
	}

	sr &= 0;
}

/*
 * if the addressing mode is not implied, the data
 * must be fetched. this function is a utility
 * function that does that.
*/
uint8_t CPU6502::fetch() {
	if(lookup[opcode].am != &CPU6502::am_imp) {
		return bus->read(addr);
	}

	return 0x00;
}

/*
 * addressing modes 
 * ~~~~~~~~~~~~~~~~
*/

/*
 * absolute
 *
 * The operand is a 16 bit address.
*/
uint8_t CPU6502::am_abs() {
	uint16_t lo = bus->read(pc++);
	uint16_t hi = bus->read(pc++);

	addr = (hi << 8) | lo;
	return 0;
}

/*
 * absolute x-indexed
 *
 * The operand is a 16 bit address. The contents
 * of the x register is added to the address.
*/
uint8_t CPU6502::am_abx() {
	uint16_t lo = bus->read(pc++);
	uint16_t hi = bus->read(pc++);

	addr = (hi << 8) | lo;
	addr += x;

	/* extra cycle required if a page is passed */
	if((addr & 0xff00) != (hi << 8)) return 1;
	return 0;
}

/*
 * absolute y-indexed
 *
 * The operand is a 16 bit address. The contents
 * of the y register is added to the address.
*/
uint8_t CPU6502::am_aby() {	
	uint16_t lo = bus->read(pc++);
	uint16_t hi = bus->read(pc++);

	addr = (hi << 8) | lo;
	addr += y;

	/* extra cycle required if a page is passed */
	if((addr & 0xff00) != (hi << 8)) return 1;
	return 0;
}

/*
 * immediate
 *
 * The operand is define at pc + 1;
*/
uint8_t CPU6502::am_imm() {	
	addr = ++pc;
	return 0;
}

/*
 * implicit
 *
 * There is no operand. The A register is used.
*/
uint8_t CPU6502::am_imp() {	
	fetched = a;
	return 0;
}

/*
 * indirect
 *
 * The supplied 16 bit address points to a 16 bit
 * value. There is a weird bug where if the low
 * byte of the supplied address is 0xff, then the
 * high byte wraps around and gives an invalid 
 * address.
*/
uint8_t CPU6502::am_ind() {	
	uint16_t lo = bus->read(pc++);
	uint16_t hi = bus->read(pc++);
	uint16_t ptr = (lo << 8) | hi;

	/* page boundary bug */
	if(lo == 0xff) {
		addr = (bus->read(ptr & 0xff00) << 8) | bus->read(ptr);
		return 0;
	}

	addr = (bus->read(ptr + 1) << 8) | bus->read(ptr);
	return 0;
}

/*
 * indirect x-indexed
 *
 * The supplied 8 bit address points to a location
 * in the zero page. That location is then read as
 * a 16 bit address and the contents of the x
 * register is added to the address. If the
 * offset causes a change in page, an extra clock
 * cycle is required.
*/
uint8_t CPU6502::am_izx() {
	uint16_t ptr = bus->read(pc++);
	uint16_t lo = bus->read((uint16_t) (ptr + (uint16_t) x) & 0x00ff);
	uint16_t hi = bus->read((uint16_t) (ptr + (uint16_t) x + 1) & 0x00ff);

	addr = (hi << 8) | lo;
	return 0;
}

/*
 * indirect y-indexed
 *
 * The supplied 8 bit address points to a location
 * in the zero page. That location is then read as
 * a 16 bit address and the contents of the y
 * register is added to the address. If the
 * offset causes a change in page, an extra clock
 * cycle is required.
*/
uint8_t CPU6502::am_izy() {	
	uint16_t ptr = bus->read(pc++);
	uint16_t lo = bus->read((uint16_t) (ptr + (uint16_t) y) & 0x00ff);
	uint16_t hi = bus->read((uint16_t) (ptr + (uint16_t) y + 1) & 0x00ff);

	addr = (hi << 8) | lo;
	return 0;
}

/*
 * relative
 *
 * used exclusively for branch instructions
 * (e.i. bne, bvs, etc). references a location
 * in memory that is in the range of -128 to
 * +128 bytes of the program counter.
*/
uint8_t CPU6502::am_rel() { 
	addr = bus->read(pc++);
	if(addr & 0x80) {
		addr |= 0xff00;
	}

	addr += pc;
	return 0;
}

/* 
 * zero page
 *
 * to save space in memory and time in cycles,
 * it is possible to fetch a variable in
 * memory with only 1 byte if it is stored in
 * the zero page region of memory (the first 
 * 128 bytes of memory)
*/

uint8_t CPU6502::am_zpg() {	
	addr = bus->read(pc++);
	addr &= 0x00ff;
	return 0;
}

/* 
 * zero page x-indexed
 *
 * to save space in memory and time in cycles,
 * it is possible to fetch a variable in
 * memory with only 1 byte if it is stored in
 * the zero page region of memory (the first 
 * 128 bytes of memory). For this addressing
 * mode, the y register is used as an offset.
*/

uint8_t CPU6502::am_zpx() {	
	addr = bus->read(pc++) + x;
	addr &= 0x00ff;
	return 0;
}

/* 
 * zero page y-indexed
 *
 * to save space in memory and time in cycles,
 * it is possible to fetch a variable in
 * memory with only 1 byte if it is stored in
 * the zero page region of memory (the first 
 * 128 bytes of memory). For this addressing
 * mode, the y register is used as an offset.

*/
uint8_t CPU6502::am_zpy() {
	addr = bus->read(pc++) + y;
	addr &= 0x00ff;
	return 0;
}

/*
 * instructions 
 * ~~~~~~~~~~~~
*/

/*
 * instruction: add w/ carry
 * function: a + m + c -> a, c
 * flags: n, z, c, v
*/
uint8_t CPU6502::op_adc() {
	uint16_t temp = (uint16_t) a + (uint16_t)fetched + (uint16_t)get_flag(C);

	set_flag(N, temp & 0x80);
	set_flag(Z, (temp & 0x00ff) == 0);
	set_flag(C, temp > 255);
	set_flag(V, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);
	
	a = temp & 0x00ff;
}

/*
 * instruction: logical and
 * function: a AND m -> a
 * flags: n, z
*/
uint8_t CPU6502::op_and() {	
	fetch();
	a &= fetched;
}

uint8_t CPU6502::op_asl() {	

}
uint8_t CPU6502::op_bcc() {	

}
uint8_t CPU6502::op_bcs() {	

}
void CPU6502::op_beq() {	

}
void CPU6502::op_bit() {	

}
void CPU6502::op_bmi() {	

}
void CPU6502::op_bne() {	

}
void CPU6502::op_bpl() {	

}
void CPU6502::op_brk() {	

}
void CPU6502::op_bvc() {	

}
void CPU6502::op_bvs() {	

}
void CPU6502::op_clc() {	

}
void CPU6502::op_cld() {	

}
void CPU6502::op_cli() {	

}
void CPU6502::op_clv() {	

}
void CPU6502::op_cmp() {	

}
void CPU6502::op_cpx() {	

}
void CPU6502::op_cpy() {	

}
void CPU6502::op_dec() {	

}
void CPU6502::op_dex() {	

}
void CPU6502::op_dey() {	

}
void CPU6502::op_eor() {	

}
void CPU6502::op_inc() {	

}
void CPU6502::op_inx() {	

}
void CPU6502::op_iny() {	

}
void CPU6502::op_jmp() {	

}
void CPU6502::op_jsr() {	

}
void CPU6502::op_lda() {	

}
void CPU6502::op_ldx() {	

}
void CPU6502::op_ldy() {	

}
void CPU6502::op_lsr() {	

}
void CPU6502::op_nop() {	

}
void CPU6502::op_ora() {	

}
void CPU6502::op_pha() {	

}
void CPU6502::op_php() {	

}
void CPU6502::op_pla() {	

}
void CPU6502::op_plp() {	

}
void CPU6502::op_rol() {	

}
void CPU6502::op_ror() {	

}
void CPU6502::op_rti() {	

}
void CPU6502::op_rts() {	

}
void CPU6502::op_sbc() {	

}
void CPU6502::op_sec() {	

}
void CPU6502::op_sed() {	

}
void CPU6502::op_sei() {	

}
void CPU6502::op_sta() {	

}
void CPU6502::op_stx() {	

}
void CPU6502::op_sty() {	

}
void CPU6502::op_tax() {	

}
void CPU6502::op_tay() {	

}
void CPU6502::op_tsx() {	

}
void CPU6502::op_txa() {	

}
void CPU6502::op_txs() {	

}
void CPU6502::op_tya() {	

}
void CPU6502::op_xxx() {	

}
