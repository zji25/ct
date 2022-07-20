// - 00; 0 01; + 10
module not_switch(in, out); 
  input in;
  output out;
  
  supply1 power; 
  supply0 ground; 
  
  pmos p1(out, power, in);
  nmos n1(out, ground, in); 
  
endmodule  


module nor_switch(a, b, out);
  input a, b;
  output out;
  wire w;
  
  supply1 power;
  supply0 ground;
  
  nmos n1(out, ground, a);
  nmos n2(out, ground, b);
  
  pmos p1(w, power, a);
  pmos p2(out, w, b);
endmodule

module nand_switch(a, b, out);
  input a, b;
  output out;
  wire w;
  
  supply1 power;
  supply0 ground;
  
  nmos n1(w, ground, b);
  nmos n2(out, w, a);
  pmos p1(out, power, a);
  pmos p2(out, power, b);
endmodule
 

module and_switch(a, b, out);
  input a, b;
  output out;
  wire w;
  
  nand_switch my_nand(a, b, w);
  not_switch my_not(w, out);
  
endmodule

module or_switch(a, b, out);
  input a, b;
  output out;
  wire w;
  nor_switch my_nor(a, b, w);
  not_switch my_not(w, out);
endmodule


module ternary_min(a0, a1, b0, b1, out0, out1);
  input a0, a1, b0, b1;
  output out0, out1;
  wire x, y, z, w, q, p;
   
  and_switch and0(a1, b1, out1);
  
  or_switch or0(a0, b0, x);
  
  or_switch or1(a0, a1, y);
  or_switch or2(y, b1, z);
  
  or_switch or3(a1, b0, w);
  or_switch or4(w, b1, q);
  
  and_switch and1(x, z, p);
  and_switch and2(p, q, out0);
endmodule 

module ternary_any(a0, a1, b0, b1, out0, out1);
  input a0, a1, b0, b1;
  output out0, out1;
  wire w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20, w21;
  
  not_switch not1(a0, w1);
  or_switch or1(w1, a1, w2);
  or_switch or2(w2, b0, w3);
  
  or_switch or3(a0, a1, w4);
  or_switch or4(w4, b1, w5);
  
  not_switch not2(b0, w6);
  or_switch or5(a0, w6, w7);
  or_switch or6(w7, b1, w8);
  
  not_switch not3(a1, w9);
  not_switch not4(b1, w10);
  or_switch or7(a0, w9, w11);
  or_switch or8(w11, b0, w12);
  or_switch or9(w12, w10, w13);
  
  and_switch and1(w3, w5, w14);
  and_switch and2(w14, w8, w15);
  and_switch and3(w15, w13, out0);
  
  
  or_switch orr1(a1, b1, w16);
  
  or_switch orr2(a0, a1, w17);
  or_switch orr3(w17, b0, w18);
  
  or_switch orr4(a0, b0, w19);
  or_switch orr5(w19, b1, w20);
  
  and_switch andd1(w16, w18, w21);
  and_switch andd2(w21, w20, out1);
  
endmodule

