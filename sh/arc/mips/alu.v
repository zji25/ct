module alu(srca, srcb, alucontrol, aluresult, zero);
    input signed [31:0] srca, srcb;
    input [2:0] alucontrol; 
    output reg signed [31:0] aluresult;
    output reg zero;
    always @(alucontrol or srca or srcb) begin
	case (alucontrol)
	0: // and
	    aluresult <= srca & srcb;
	1: // or
	    aluresult <= srca | srcb;
	2: // +
	    aluresult <= srca + srcb;
	3: // not used
	    aluresult <= srca | srcb;
	4:
	    aluresult <= srca & ~(srcb);
	5:
	    aluresult <= srca | ~(srcb);
	6: // -
	    aluresult <= srca - srcb;
	7: // slt
	    aluresult <= srca < srcb ? 32'b1 : 32'b0;	
	endcase
    end
	
    always @(aluresult) begin
	if (aluresult == 32'b0) begin
	    zero <= 1'b1;
	end else begin
	    zero <= 1'b0;
	end
    end
endmodule