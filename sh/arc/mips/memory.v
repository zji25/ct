module data_memory(a, we, clk, wd, rd);
    input we, clk;
    input [31:0] a;
    input [31:0] wd;
    output [31:0] rd;
    reg [31:0] ram[0:1048575];	
    integer i;
    initial begin
      for (i = 0; i < 1048576; i += 1)  begin
        ram[i] = 32'b0;
      end
    end
    always @(posedge clk) begin
        if (we) begin
            ram[a / 4] <= wd; 
        end
    end
    assign rd = ram[a / 4];	
endmodule

module instruction_memory(a, rd);
  input [31:0] a;
  output [31:0] rd;
  reg [31:0] ram[0:1048575];
  integer i;
  initial begin
    for (i = 0; i < 1048576; i += 1)  begin
      ram[i] = 32'b0;
    end
    $readmemb("instructions.dat", ram);
  end
  assign rd = ram[a / 4];	
endmodule