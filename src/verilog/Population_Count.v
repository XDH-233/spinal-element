

module Population_Count
#(
    parameter WORD_WIDTH        = 0,

    // Don't set at instantiation (see above)
    parameter POPCOUNT_WIDTH    = WORD_WIDTH
)
(
    input   wire    [WORD_WIDTH-1:0]        word_in,
    output  reg     [POPCOUNT_WIDTH-1:0]    count_out
);

    initial begin
        count_out = {POPCOUNT_WIDTH{1'b0}};
    end



//    (* ramstyle = "logic" *)        // Quartus
    (* ram_style = "distributed" *) // Vivado

    reg [1:0] popcount2bits [0:3];

    initial begin
        popcount2bits[0] = 2'd0;
        popcount2bits[1] = 2'd1;
        popcount2bits[2] = 2'd1;
        popcount2bits[3] = 2'd2;
    end



    localparam PAIR_COUNT       = WORD_WIDTH / 2;
    localparam PAIR_WORD_WIDTH  = PAIR_COUNT * 2;
    localparam PAD_WIDTH        = POPCOUNT_WIDTH > 2 ? POPCOUNT_WIDTH - 2 : POPCOUNT_WIDTH;
    localparam PAD              = {PAD_WIDTH{1'b0}};



    reg [PAIR_WORD_WIDTH-1:0]               paircount   = {PAIR_WORD_WIDTH{1'b0}};
    // verilator lint_off UNOPTFLAT
    reg [(POPCOUNT_WIDTH*PAIR_COUNT)-1:0]   popcount    = {POPCOUNT_WIDTH*PAIR_COUNT{1'b0}};
    // verilator lint_on  UNOPTFLAT



    localparam WORD_WIDTH_IS_ODD = (WORD_WIDTH % 2) == 1;



    integer i;

    always @(*) begin
        paircount[0 +: 2]               = popcount2bits[word_in[0 +: 2]];
        // This is decided at elaboration, but the linter doesn't know that.
        if (PAD_WIDTH == POPCOUNT_WIDTH) begin
            popcount[0 +: POPCOUNT_WIDTH]   = paircount[0 +: 2];
        end
        else begin
            // verilator lint_off WIDTH
            popcount[0 +: POPCOUNT_WIDTH]   = {PAD,paircount[0 +: 2]};
            // verilator lint_on  WIDTH
        end


        for(i=1; i < PAIR_COUNT; i=i+1) begin : per_paircount
            paircount[2*i +: 2]                          = popcount2bits[word_in[2*i +: 2]];
            // This is decided at elaboration, but the linter doesn't know that.
            if (PAD_WIDTH == POPCOUNT_WIDTH) begin
                popcount[POPCOUNT_WIDTH*i +: POPCOUNT_WIDTH] = paircount[2*i +: 2] + popcount[POPCOUNT_WIDTH*(i-1) +: POPCOUNT_WIDTH];
            end
            else begin
                // verilator lint_off WIDTH
                popcount[POPCOUNT_WIDTH*i +: POPCOUNT_WIDTH] = {PAD,paircount[2*i +: 2]} + popcount[POPCOUNT_WIDTH*(i-1) +: POPCOUNT_WIDTH];
                // verilator lint_on  WIDTH
            end
        end


        if (WORD_WIDTH_IS_ODD == 1'b1) begin
            popcount[POPCOUNT_WIDTH*(PAIR_COUNT-1) +: POPCOUNT_WIDTH] = popcount[POPCOUNT_WIDTH*(PAIR_COUNT-1) +: POPCOUNT_WIDTH] + {{POPCOUNT_WIDTH-1{1'b0}},word_in[WORD_WIDTH-1]};
        end

// Then the last popcount is the total for the whole input word.

        count_out = popcount[POPCOUNT_WIDTH*(PAIR_COUNT-1) +: POPCOUNT_WIDTH];
    end

endmodule

