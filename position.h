/*
MIT License

Copyright (c) 2017 CPirc

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#ifndef POSITION_H
#define POSITION_H

#include "types.h"
#include "bitboard.h"
#include "tt.h"

/* A chess position. */
struct Position {
    std::uint64_t bitboards[4]; // Bitboards containing piece locations, plus a side to move bitboard.
    std::uint8_t castle;        // Castling rights.
    bool flipped;               // Has the board been flipped or not?
    Square epsq;                // En passant square.
    char fifty;                 // Fifty-move rule counter.
    std::uint64_t hash_key;     // Zobrist hash of the current position.
};

extern std::uint64_t side_keys[2];
extern std::uint64_t castle_keys[16];
extern std::uint64_t piece_sq_keys[2][6][64];

extern void initialize_keys();

/* Extract data from a FEN string to a Position struct */
extern void parse_fen_to_position(const char* fen_str, Position& pos);

extern void run_fen_parser_tests();

/* Get the type of piece on a square(bitboard of the square) */
template<Piece>
inline Piece get_piece_on_square(const Position& pos, const std::uint64_t sq)
{
    const Piece from_qbb[8] = {
        NO_PIECE, PAWN, KNIGHT, BISHOP,
        ROOK, QUEEN, KING, NO_PIECE
    };

    uint64_t piece =  (pos.bitboards[PBQ] & sq)       |
                     ((pos.bitboards[NBK] & sq) << 1) |
                     ((pos.bitboards[RQK] & sq) << 2);

    piece >>= lsb(sq);

    return from_qbb[piece];
}

/* Get the type of piece on a square */
template<Piece piece = PAWN>
inline Piece get_piece_on_square(const Position& pos, const Square sq)
{
    return get_piece_on_square<piece>(pos, (1ULL << sq));
}

/* Get a piece bitboard. */
inline std::uint64_t get_piece(const Position& pos, const Piece p)
{
    switch (p) {
    case PAWN:
        return pos.bitboards[PBQ] & ~pos.bitboards[NBK] & ~pos.bitboards[RQK];
    case KNIGHT:
        return ~pos.bitboards[PBQ] & pos.bitboards[NBK] & ~pos.bitboards[RQK];
    case BISHOP:
        return pos.bitboards[PBQ] & pos.bitboards[NBK];
    case ROOK:
        return ~pos.bitboards[PBQ] & ~pos.bitboards[NBK] & pos.bitboards[RQK];
    case QUEEN:
        return pos.bitboards[PBQ] & pos.bitboards[RQK];
    case KING:
        return pos.bitboards[NBK] & pos.bitboards[RQK];
    case NO_PIECE:
        return 0;
    }
    return 0;
}

/* Get the board occupancy. */
inline std::uint64_t get_occupancy(const Position& pos)
{
    return pos.bitboards[PBQ] | pos.bitboards[NBK] | pos.bitboards[RQK];
}

/* Get a colour bitboard. */
inline std::uint64_t get_colour(const Position& pos, const Colour c)
{
    if (c == US) {
        return pos.bitboards[STM];
    }
    return get_occupancy(pos) ^ pos.bitboards[STM];
}

/* Get a piece bitboard of a colour. */
inline std::uint64_t get_piece(const Position& b, const Piece p, const Colour c)
{
    return get_piece(b, p) & get_colour(b, c);
}

/* Updates the position by moving piece from 'from' to 'to' */
inline void move_piece(Position& pos, const Square from, const Square to,
                      const Piece piece, const Colour colour)
{
    assert(from != to);
    std::uint64_t from_to = (1ULL << from) ^ (1ULL << to);
    switch (piece) {
    case PAWN:
        pos.bitboards[PBQ] ^= from_to;
        break;
    case KNIGHT:
        pos.bitboards[NBK] ^= from_to;
        break;
    case BISHOP:
        pos.bitboards[PBQ] ^= from_to;
        pos.bitboards[NBK] ^= from_to;
        break;
    case ROOK:
        pos.bitboards[RQK] ^= from_to;
        break;
    case QUEEN:
        pos.bitboards[PBQ] ^= from_to;
        pos.bitboards[RQK] ^= from_to;
        break;
    case KING:
        pos.bitboards[NBK] ^= from_to;
        pos.bitboards[RQK] ^= from_to;
        break;
    }
    if (colour == US)
        pos.bitboards[STM] ^= from_to;
    pos.hash_key       ^= piece_sq_keys[colour][piece][from]
                       ^  piece_sq_keys[colour][piece][to];
}

/* Updates the position by putting piece on 'to' */
inline void put_piece(Position& pos, const Square to, const Piece piece,
                      const Colour colour)
{
    std::uint64_t to_bit = (1ULL << to);
    switch (piece) {
    case PAWN:
        pos.bitboards[PBQ] |= to_bit;
        break;
    case KNIGHT:
        pos.bitboards[NBK] |= to_bit;
        break;
    case BISHOP:
        pos.bitboards[PBQ] |= to_bit;
        pos.bitboards[NBK] |= to_bit;
        break;
    case ROOK:
        pos.bitboards[RQK] |= to_bit;
        break;
    case QUEEN:
        pos.bitboards[PBQ] |= to_bit;
        pos.bitboards[RQK] |= to_bit;
        break;
    case KING:
        pos.bitboards[NBK] |= to_bit;
        pos.bitboards[RQK] |= to_bit;
        break;
    }
    if (colour == US)
        pos.bitboards[STM] |= to_bit;
    pos.hash_key       ^= piece_sq_keys[colour][piece][to];
}

/* Updates the position by removing piece from 'from' */
inline void remove_piece(Position& pos, const Square from, const Piece piece,
                      const Colour colour)
{
    std::uint64_t from_bit = (1ULL << from);
    switch (piece) {
    case PAWN:
        pos.bitboards[PBQ] &= ~from_bit;
        break;
    case KNIGHT:
        pos.bitboards[NBK] &= ~from_bit;
        break;
    case BISHOP:
        pos.bitboards[PBQ] &= ~from_bit;
        pos.bitboards[NBK] &= ~from_bit;
        break;
    case ROOK:
        pos.bitboards[RQK] &= ~from_bit;
        break;
    case QUEEN:
        pos.bitboards[PBQ] &= ~from_bit;
        pos.bitboards[RQK] &= ~from_bit;
        break;
    case KING:
        pos.bitboards[NBK] &= ~from_bit;
        pos.bitboards[RQK] &= ~from_bit;
        break;
    }

    if (colour == US)
        pos.bitboards[STM] &= ~from_bit;
    pos.hash_key       ^= piece_sq_keys[colour][piece][from];
}

/* Get any piece attacks to a square. */
template<Piece p = PAWN>
inline std::uint64_t attacks_to(const Position& pos, const Square sq, const std::uint64_t occ, const Colour by)
{
    return (attacks<p>(sq, occ) & get_piece(pos, p)) | attacks_to<p+1>(pos, sq, occ, by);
}

template<>
inline std::uint64_t attacks_to<PAWN>(const Position& pos, const Square sq, const std::uint64_t occ, const Colour by)
{
    return (pawn_attacks(sq, by) & get_piece(pos, PAWN)) | attacks_to<KNIGHT>(pos, sq, occ, by);
}

/* Required for C++11. C++17's if constexpr would solve this. */
template<>
inline std::uint64_t attacks_to<KING>(const Position& pos, const Square sq, const std::uint64_t occ, const Colour)
{
    return attacks<KING>(sq, occ) & get_piece(pos, KING);
}

/* Checks to see if c is in check */
inline bool is_checked(const Position& pos, const Colour c)
{
    Colour by = (c == US ? THEM : US);
    return (attacks_to(pos, lsb(get_piece(pos, KING, c)), get_occupancy(pos), c) & get_colour(pos, by)) > std::uint64_t(0);
}

/* Flips the position */
inline void flip_position(Position& pos)
{
    // Create new colour bitboard
    pos.bitboards[STM] = get_colour(pos, THEM);

    // Flip piece bitboards and colour bitboard
    std::uint64_t* curr;
    for (curr = pos.bitboards; curr < pos.bitboards + 4; ++curr)
        *curr = __builtin_bswap64(*curr);

    // Flip epsq
    if (pos.epsq != INVALID_SQUARE)
        pos.epsq = Square(int(pos.epsq) ^ 56);

    // Flip castling rights
    std::uint8_t tmp2 = (pos.castle & 3) << 2;
    pos.castle >>= 2;
    pos.castle ^= tmp2;

    // Flip flipped
    pos.flipped = !pos.flipped;
}

extern std::uint64_t perft(const Position& pos, int depth);
extern void run_perft_tests();

#endif
