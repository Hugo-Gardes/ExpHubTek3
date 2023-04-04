/*
** EPITECH PROJECT, 2022
** my_nes
** File description:
** bus.cpp
*/

#include "bus.hpp"

Bus::Bus()
{
    for (auto &i : ram)
        i = 0x00;

    cpu.ConnectBus(this);
}

Bus::~Bus()
{
}

void Bus::write(uint16_t addr, uint8_t data)
{
    if (addr >= 0x0000 && addr <= 0xFFFF)
        ram[addr] = data;
}

uint8_t Bus::read(uint16_t addr, bool bReadOnly)
{
    if (addr >= 0x0000 && addr <= 0xFFFF)
        return ram[addr];
    return 0x00;
}

std::ostream &operator<<(std::ostream &os, Bus &bus)
{
    os << bus.cpu;
    return (os);
}
