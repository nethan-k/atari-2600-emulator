#include "bus.h"

Bus::Bus() {

}

Bus::~Bus() {

}

uint8_t Bus::read(uint16_t addr) {

	/* TIA handler (0x0000-0x002C: TIA Write, 0x0030-0x003D: TIA Read) */
	if(addr >= 0x0000 && addr <= 0x003D) {
		return tia.read(addr);
	}

	/* RIOT RAM handler (0x0080-0x00FF) */
	if(addr >= 0x0080 && addr <= 0x00FF) {
		addr -= 0x0080;
		return riot.read_ram(addr);
	}

	/* RIOT I/O & Timer handler (0x0280-0x0297) */
	if(addr >= 0x0280 && addr <= 0x0297) {
		addr -= 0x0280;
		return riot.read_util(addr);
	}

	/* ROM handler (0xF000-0xFFFF) */
	if(addr >= 0xF000 && addr <= 0xFFFF) {
		return rom[addr-0xF000];
	}

	return 0x00;
}

void Bus::write(uint16_t addr, uint8_t val) {

	/* TIA handler (0x0000-0x002C: TIA Write, 0x0030-0x003D: TIA Read) */
	if(addr >= 0x0000 && addr <= 0x003D) {
		tia.write(addr, val);
		return;
	}

	/* RIOT RAM handler (0x0080-0x00FF) */
	if(addr >= 0x0080 && addr <= 0x00FF) {
		addr -= 0x0080;
		return riot.write_ram(addr, val);
	}

	/* RIOT I/O & Timer handler (0x0280-0x0297) */
	if(addr < 0x0280 && addr > 0x0297) {
		addr -= 0x0280;
		riot.write_util(addr, val);
		return;
	}
}
