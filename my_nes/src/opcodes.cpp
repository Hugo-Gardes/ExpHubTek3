#include "olc6502.hpp"
#include "Bus.hpp"

uint8_t olc6502::ADC()
{
    fetch();
    uint16_t temp = (uint16_t)a + (uint16_t)fetched + (uint16_t)get_flag(C);
    set_flag(C, temp > 255);
    set_flag(Z, (temp & 0x00FF) == 0);
    set_flag(V, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);
    set_flag(N, temp & 0x80);
    a = temp & 0x00FF;
    return 1;
}

uint8_t olc6502::SBC()
{
    fetch();
    uint16_t value = ((uint16_t)fetched) ^ 0x00FF;
    uint16_t temp = (uint16_t)a + value + (uint16_t)get_flag(C);
    set_flag(C, temp & 0xFF00);
    set_flag(Z, ((temp & 0x00FF) == 0));
    set_flag(V, (temp ^ (uint16_t)a) & (temp ^ value) & 0x0080);
    set_flag(N, temp & 0x0080);
    a = temp & 0x00FF;
    return 1;
}

uint8_t olc6502::AND()
{
    fetch();
    a = a & fetched;
    set_flag(Z, a == 0x00);
    set_flag(N, a & 0x80);
    return 1;
}

uint8_t olc6502::ASL()
{
    fetch();
    uint16_t temp = (uint16_t)fetched << 1;
    set_flag(C, (temp & 0xFF00) > 0);
    set_flag(Z, (temp & 0x00FF) == 0x00);
    set_flag(N, temp & 0x80);
    if (lookup[opcode].addrmode == &olc6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

uint8_t olc6502::BCC()
{
    if (get_flag(C) == 0)
    {
        cycles++;
        addr_abs = pc + addr_rel;
        if((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

uint8_t olc6502::BCS()
{
    if (get_flag(C) == 1)
    {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

uint8_t olc6502::BEQ()
{
    if (get_flag(Z) == 1)
    {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

uint8_t olc6502::BIT()
{
    fetch();
    uint16_t temp = a & fetched;
    set_flag(Z, (temp & 0x00FF) == 0x00);
    set_flag(N, fetched & (1 << 7));
    set_flag(V, fetched & (1 << 6));
    return 0;
}

uint8_t olc6502::BMI()
{
    if (get_flag(N) == 1)
    {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

uint8_t olc6502::BNE()
{
    if (get_flag(Z) == 0)
    {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

uint8_t olc6502::BPL()
{
    if (get_flag(N) == 0)
    {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

uint8_t olc6502::BRK()
{
    pc++;
    set_flag(I, 1);
    write(0x0100 + stkp, (pc >> 8) & 0x00FF);
    stkp--;
    write(0x0100 + stkp, pc & 0x00FF);
    stkp--;
    set_flag(B, 1);
    write(0x0100 + stkp, status);
    stkp--;
    set_flag(B, 0);
    pc = (uint16_t)read(0xFFFE) | ((uint16_t)read(0xFFFF) << 8);
    return 0;
}

uint8_t olc6502::BVC()
{
    if (get_flag(V) == 0)
    {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

uint8_t olc6502::BVS()
{
    if (get_flag(V) == 1)
    {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

uint8_t olc6502::CLC()
{
    set_flag(C, false);
    return 0;
}

uint8_t olc6502::CLD()
{
    set_flag(D, false);
    return 0;
}

uint8_t olc6502::CLI()
{
    set_flag(I, false);
    return 0;
}

uint8_t olc6502::CLV()
{
    set_flag(V, false);
    return 0;
}

uint8_t olc6502::CMP()
{
    fetch();
    uint16_t temp = (uint16_t)a - (uint16_t)fetched;
    set_flag(C, a >= fetched);
    set_flag(Z, (temp & 0x00FF) == 0x0000);
    set_flag(N, temp & 0x0080);
    return 1;
}

uint8_t olc6502::CPX()
{
    fetch();
    uint16_t temp = (uint16_t)x - (uint16_t)fetched;
    set_flag(C, x >= fetched);
    set_flag(Z, (temp & 0x00FF) == 0x0000);
    set_flag(N, temp & 0x0080);
    return 0;
}

uint8_t olc6502::CPY()
{
    fetch();
    uint16_t temp = (uint16_t)y - (uint16_t)fetched;
    set_flag(C, y >= fetched);
    set_flag(Z, (temp & 0x00FF) == 0x0000);
    set_flag(N, temp & 0x0080);
    return 0;
}

uint8_t olc6502::DEC()
{
    fetch();
    uint16_t temp = fetched - 1;
    write(addr_abs, temp & 0x00FF);
    set_flag(Z, (temp & 0x00FF) == 0x0000);
    set_flag(N, temp & 0x0080);
    return 0;
}

uint8_t olc6502::DEX()
{
    x--;
    set_flag(Z, x == 0x00);
    set_flag(N, x & 0x80);
    return 0;
}

uint8_t olc6502::DEY()
{
    y--;
    set_flag(Z, y == 0x00);
    set_flag(N, y & 0x80);
    return 0;
}

uint8_t olc6502::EOR()
{
    fetch();
    a = a ^ fetched;
    set_flag(Z, a == 0x00);
    set_flag(N, a & 0x80);
    return 1;
}

uint8_t olc6502::INC()
{
    fetch();
    uint16_t temp = fetched + 1;
    write(addr_abs, temp & 0x00FF);
    set_flag(Z, (temp & 0x00FF) == 0x0000);
    set_flag(N, temp & 0x0080);
    return 0;
}

uint8_t olc6502::INX()
{
    x++;
    set_flag(Z, x == 0x00);
    set_flag(N, x & 0x80);
    return 0;
}

uint8_t olc6502::INY()
{
    y++;
    set_flag(Z, y == 0x00);
    set_flag(N, y & 0x80);
    return 0;
}

uint8_t olc6502::JMP()
{
    pc = addr_abs;
    return 0;
}

uint8_t olc6502::JSR()
{
    pc--;

    write(0x0100 + stkp, (pc >> 8) & 0x00FF);
    stkp--;
    write(0x0100 + stkp, pc & 0x00FF);
    stkp--;

    pc = addr_abs;
    return 0;
}

uint8_t olc6502::LDA()
{
    fetch();
    a = fetched;
    set_flag(Z, a == 0x00);
    set_flag(N, a & 0x80);
    return 1;
}

uint8_t olc6502::LDX()
{
    fetch();
    x = fetched;
    set_flag(Z, x == 0x00);
    set_flag(N, x & 0x80);
    return 1;
}

uint8_t olc6502::LDY()
{
    fetch();
    y = fetched;
    set_flag(Z, y == 0x00);
    set_flag(N, y & 0x80);
    return 1;
}

uint8_t olc6502::LSR()
{
    fetch();
    set_flag(C, fetched & 0x0001);
    uint16_t temp = fetched >> 1;
    set_flag(Z, (temp & 0x00FF) == 0x0000);
    set_flag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &olc6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

uint8_t olc6502::NOP()
{
    switch (opcode) {
    case 0x1C:
    case 0x3C:
    case 0x5C:
    case 0x7C:
    case 0xDC:
    case 0xFC:
        return 1;
        break;
    }
    return 0;
}

uint8_t olc6502::ORA()
{
    fetch();
    a = a | fetched;
    set_flag(Z, a == 0x00);
    set_flag(N, a & 0x80);
    return 1;
}

uint8_t olc6502::PHA()
{
    write(0x0100 + stkp, a);
    stkp--;
    return 0;
}

uint8_t olc6502::PHP()
{
    write(0x0100 + stkp, status | B | U);
    set_flag(B, 0);
    set_flag(U, 0);
    stkp--;
    return 0;
}

uint8_t olc6502::PLA()
{
    stkp++;
    a = read(0x0100 + stkp);
    set_flag(Z, a == 0x00);
    set_flag(N, a & 0x80);
    return 0;
}

uint8_t olc6502::PLP()
{
    stkp++;
    status = read(0x0100 + stkp);
    set_flag(U, 1);
    return 0;
}

uint8_t olc6502::ROL()
{
    fetch();
    uint16_t temp = (uint16_t)(fetched << 1) | get_flag(C);
    set_flag(C, temp & 0xFF00);
    set_flag(Z, (temp & 0x00FF) == 0x0000);
    set_flag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &olc6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

uint8_t olc6502::ROR()
{
    fetch();
    uint16_t temp = (uint16_t)(get_flag(C) << 7) | (fetched >> 1);
    set_flag(C, fetched & 0x01);
    set_flag(Z, (temp & 0x00FF) == 0x00);
    set_flag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &olc6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

uint8_t olc6502::RTS()
{
    stkp++;
    pc = (uint16_t)read(0x0100 + stkp);
    stkp++;
    pc |= (uint16_t)read(0x0100 + stkp) << 8;
    pc++;
    return 0;
}

uint8_t olc6502::SEC()
{
    set_flag(C, true);
    return 0;
}

uint8_t olc6502::SED()
{
    set_flag(D, true);
    return 0;
}

uint8_t olc6502::SEI()
{
    set_flag(I, true);
    return 0;
}

uint8_t olc6502::STA()
{
    write(addr_abs, a);
    return 0;
}

uint8_t olc6502::STX()
{
    write(addr_abs, x);
    return 0;
}

uint8_t olc6502::STY()
{
    write(addr_abs, y);
    return 0;
}

uint8_t olc6502::TAX()
{
    x = a;
    set_flag(Z, x == 0x00);
    set_flag(N, x & 0x80);
    return 0;
}

uint8_t olc6502::TAY()
{
    y = a;
    set_flag(Z, y == 0x00);
    set_flag(N, y & 0x80);
    return 0;
}

uint8_t olc6502::TSX()
{
    x = stkp;
    set_flag(Z, x == 0x00);
    set_flag(N, x & 0x80);
    return 0;
}

uint8_t olc6502::TXA()
{
    a = x;
    set_flag(Z, a == 0x00);
    set_flag(N, a & 0x80);
    return 0;
}

uint8_t olc6502::TXS()
{
    stkp = x;
    return 0;
}

uint8_t olc6502::TYA()
{
    a = y;
    set_flag(Z, a == 0x00);
    set_flag(N, a & 0x80);
    return 0;
}

uint8_t olc6502::XXX()
{
    return 0;
}
