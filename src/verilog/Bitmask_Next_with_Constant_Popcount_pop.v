

module Bitmask_Next_with_Constant_Popcount_pop
#(
    parameter WORD_WIDTH = 0
)
(
    input   wire    [WORD_WIDTH-1:0]    word_in,
    output  reg     [WORD_WIDTH-1:0]    word_out
);

// First, let's define some constants used throughout. Rather than expect
// the simulator/synthesizer to get the Verilog spec right and extend
// integers correctly, we defensively specify the entire word.

    localparam ZERO = {WORD_WIDTH{1'b0}};
    localparam ONE  = {{WORD_WIDTH-1{1'b0}},1'b1};
    localparam TWO  = {{WORD_WIDTH-2{1'b0}},2'b10};

    initial begin
        word_out = ZERO;
    end

// We find the least-significant bit set in the bitmask.

    wire [WORD_WIDTH-1:0] smallest;

    Bitmask_Isolate_Rightmost_1_Bit
    #(
        .WORD_WIDTH (WORD_WIDTH)
    )
    find_smallest
    (
        .word_in    (word_in),
        .word_out   (smallest)
    );



    wire [WORD_WIDTH-1:0]   ripple;
    wire                    ripple_carry_out;

    Adder_Subtractor_Binary
    #(
        .WORD_WIDTH (WORD_WIDTH)
    )
    calc_ripple
    (
        .add_sub    (1'b0), // 0/1 -> A+B/A-B
        .carry_in   (1'b0),
        .A          (word_in),
        .B          (smallest),
        .sum        (ripple),
        .carry_out  (ripple_carry_out),
        // verilator lint_off PINCONNECTEMPTY
        .carries    (),
        .overflow   ()
        // verilator lint_on  PINCONNECTEMPTY
    );



    wire [WORD_WIDTH-1:0] changed_bits;

    Hamming_Distance
    #(
        .WORD_WIDTH     (WORD_WIDTH)
    )
    calc_changed_bits
    (
        .word_A           (word_in),
        .word_B           (ripple),
        .distance         (changed_bits)
    );

// We need some corrections to the Hamming Distance, which are explained
// later. If we have not reached the left end of the word, then we need
// a correction of two, else zero.

    reg [WORD_WIDTH-1:0] distance_adjustment = ZERO;

    always @(*) begin
        distance_adjustment = (ripple_carry_out == 1'b1) ? ZERO : TWO;
    end


    wire [WORD_WIDTH-1:0] adjusted_distance;

    Adder_Subtractor_Binary
    #(
        .WORD_WIDTH (WORD_WIDTH)
    )
    calc_adjusted_distance
    (
        .add_sub    (1'b1), // 0/1 -> A+B/A-B
        .carry_in   (1'b0),
        .A          (changed_bits),
        .B          (distance_adjustment),
        .sum        (adjusted_distance),
        // verilator lint_off PINCONNECTEMPTY
        .carry_out  (),
        .carries    (),
        .overflow   ()
        // verilator lint_on  PINCONNECTEMPTY
    );

// To rebuild any lost set bits, we left-shift a 1 bit by the corrected
// Hamming Distance....

    wire [WORD_WIDTH-1:0] shifted_one;

    Bit_Shifter
    #(
        .WORD_WIDTH         (WORD_WIDTH)
    )
    calc_shifted_ones
    (
        .word_in_left       (ZERO),
        .word_in            (ONE),
        .word_in_right      (ZERO),

        .shift_amount       (adjusted_distance),
        .shift_direction    (1'b0), // 0/1 -> left/right

        // verilator lint_off PINCONNECTEMPTY
        .word_out_left      (),
        .word_out           (shifted_one),
        .word_out_right     ()
        // verilator lint_on  PINCONNECTEMPTY
    );



    wire [WORD_WIDTH-1:0] lost_ones;

    Adder_Subtractor_Binary
    #(
        .WORD_WIDTH (WORD_WIDTH)
    )
    calc_lost_ones
    (
        .add_sub    (1'b1), // 0/1 -> A+B/A-B
        .carry_in   (1'b0),
        .A          (shifted_one),
        .B          (ONE),
        .sum        (lost_ones),
        // verilator lint_off PINCONNECTEMPTY
        .carry_out  (),
        .carries    (),
        .overflow   ()
        // verilator lint_on  PINCONNECTEMPTY
    );



    always @(*) begin
        word_out = ripple | lost_ones;
    end

endmodule

