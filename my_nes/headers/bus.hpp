/*
** EPITECH PROJECT, 2022
** my_nes
** File description:
** bus.hpp
*/

#pragma once
#include <cstdint>
#include <array>
#include "olc6502.hpp"

class Bus
{
public:
    Bus();
    ~Bus();

// CPU / RAM
    olc6502 cpu;
    std::array<uint8_t, 64 * 1024> ram;

// CPU can read and write to bus
    void write(uint16_t addr, uint8_t data);
    uint8_t read(uint16_t addr, bool bReadOnly = false);
};

std::ostream &operator<<(std::ostream &os, Bus &bus);