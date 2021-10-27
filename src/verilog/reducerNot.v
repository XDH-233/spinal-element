module reducerNot(
    input [7:0] data_in,
    output NOR,
    output NXOR,
    output NAND,
    output N_AND,
    output N_OR,
    output N_XOR
);
    assign NAND = ~&data_in;
    assign NOR = ~|data_in;
    assign NXOR = ~^data_in;
    assign N_XOR = ~(^data_in);
    assign N_AND = ~(&data_in);
    assign N_OR = ~(|data_in);

endmodule