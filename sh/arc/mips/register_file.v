module register_file(clk, we3, a1, a2, a3, wd3, rd1, rd2);
    input clk, we3;
    input [4:0] a1, a2, a3;
    input [31:0] wd3;
    output [31:0] rd1, rd2;
    reg [31:0] rg[0:1048575];
    integer i;
    initial begin
      for (i = 0; i < 1048576; i += 1) begin
        rg[i] = 32'b0;
      end
    end
    assign rd1 = rg[a1];
    assign rd2 = rg[a2];
    always @(posedge clk) begin
        if (we3) begin
            rg[a3] = wd3;
        end
    end
endmodule