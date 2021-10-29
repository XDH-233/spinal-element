module Bit_Shifter
#(
    parameter   WORD_WIDTH  = 0
)
(
    input   wire    [WORD_WIDTH-1:0]    word_in_left,
    input   wire    [WORD_WIDTH-1:0]    word_in,
    input   wire    [WORD_WIDTH-1:0]    word_in_right,

    input   wire    [WORD_WIDTH-1:0]    shift_amount,
    input   wire                        shift_direction, // 0/1 -> left/right

    output  reg     [WORD_WIDTH-1:0]    word_out_left,
    output  reg     [WORD_WIDTH-1:0]    word_out,
    output  reg     [WORD_WIDTH-1:0]    word_out_right
);

// Let's document the shift direction convention again here, and define our
// initial values for the outputs and the intermediate result.

    localparam  LEFT_SHIFT  = 1'b0;
    localparam  RIGHT_SHIFT = 1'b1;

    localparam  TOTAL_WIDTH = WORD_WIDTH * 3;
    localparam  TOTAL_ZERO  = {TOTAL_WIDTH{1'b0}};
    localparam  WORD_ZERO   = {WORD_WIDTH{1'b0}};

    initial begin
        word_out_left    = WORD_ZERO;
        word_out         = WORD_ZERO;
        word_out_right   = WORD_ZERO;
    end

    reg [TOTAL_WIDTH-1:0] word_in_total = TOTAL_ZERO;



    always @(*) begin
        word_in_total = {word_in_left, word_in, word_in_right};
        {word_out_left, word_out, word_out_right} = (shift_direction == LEFT_SHIFT) ? word_in_total << shift_amount : word_in_total >> shift_amount;
    end

endmodule
