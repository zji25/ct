`include "alu.v"
`include "control_unit.v"
`include "util.v"

module mips_cpu(clk, instruction_memory_a, instruction_memory_rd, data_memory_a, data_memory_rd, data_memory_we, data_memory_wd,
                register_a1, register_a2, register_a3, register_we3, register_wd3, register_rd1, register_rd2);
  input clk;
  output data_memory_we;
  output [31:0] data_memory_wd;
  output reg [31:0] instruction_memory_a;
  output [31:0] data_memory_a;
  inout [31:0] instruction_memory_rd, data_memory_rd;
  output register_we3;
  output [4:0] register_a1, register_a2, register_a3;
  output [31:0] register_wd3;
  inout [31:0] register_rd1, register_rd2;
  wire reg inst, memtoreg, memwrite, branch, alusrc, regdst, regwrite;
  wire[2:0] alucontrol;
  wire[31:0] srca, srcb, sememrd, aluresult, pcccc;
  wire zero, gg;
  initial begin
    instruction_memory_a <= 32'b0;
  end
  always @(posedge clk) begin
  	instruction_memory_a <= pcccc;
  end
  control_unit cu(instruction_memory_rd[31:26], instruction_memory_rd[5:0], memtoreg, data_memory_we, branch, alusrc, regdst, register_we3, alucontrol);
  assign register_a1 = instruction_memory_rd[25:21];
  assign register_a2 = instruction_memory_rd[20:16];
  mux2_5 mx25(instruction_memory_rd[20:16], instruction_memory_rd[15:11], regdst, register_a3);
  sign_extend se(instruction_memory_rd[15:0], sememrd);
  assign srca = register_rd1;
  mux2_32 srcb_mux(register_rd2, sememrd, alusrc, srcb);
  alu aaaluuu(srca, srcb, alucontrol, aluresult, zero);
  assign data_memory_wd = register_rd2;
  assign data_memory_a = aluresult;
  wire[31:0] regw3, tttt, sh, pfd;
  mux2_32 mxxxxxxxxxx(aluresult, data_memory_rd, memtoreg, regw3);
  assign register_wd3 = regw3;
  adder adddd(instruction_memory_a, 4, tttt);
  shl_2 shhhh(sememrd, sh);
  adder addd(sh, tttt, pfd);
  assign gg = branch & zero;
  mux2_32 mxxxxx(tttt, pfd, gg, pcccc);
endmodule
