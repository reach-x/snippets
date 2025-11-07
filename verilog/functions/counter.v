// Simple counter in Verilog

module counter (
    input wire clk,
    input wire reset,
    output reg [3:0] count
);

always @(posedge clk or posedge reset) begin
    if (reset)
        count <= 4'b0000;
    else
        count <= count + 1;
end

endmodule

// Testbench
module counter_tb;
    reg clk, reset;
    wire [3:0] count;

    counter uut (
        .clk(clk),
        .reset(reset),
        .count(count)
    );

    initial begin
        clk = 0;
        forever #5 clk = ~clk;
    end

    initial begin
        reset = 1;
        #10 reset = 0;
        #100 $finish;
    end

    initial begin
        $monitor("Time=%0t Reset=%b Count=%d", $time, reset, count);
    end
endmodule
