with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Graph;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;
with Random;

procedure Main is
   N       : Positive;
   D       : Natural;
   K       : Natural;
   H       : Natural;
   G       : Graph.Graph;
   Packets : Graph.Packet_Vector.Vector;
begin
   Ada.Text_IO.Put ("Give the number of nodes: ");
   Ada.Integer_Text_IO.Get (N);
   Ada.Text_IO.Put ("Give the number of shortcuts: ");
   Ada.Integer_Text_IO.Get (D);
   Ada.Text_IO.Put ("Give the number of packets to be send: ");
   Ada.Integer_Text_IO.Get (K);
   Ada.Text_IO.Put ("Give the HP: ");
   Ada.Integer_Text_IO.Get (H);

   G := Graph.Graph_Generator (N, D);
   Graph.Print_Graph (G);

   declare
      Printer : Graph.Printer := new Graph.Proper_Printer (K);
      Hunter  : Graph.Hunter  := new Graph.Proper_Hunter (0);
   begin
      for V of G.Nodes loop
         V.My_Vertice := new Graph.Proper_Vertice (V, Printer);
      end loop;

      for I in 0 .. K - 1 loop
         delay Random.Rand_Time;
         declare
            Packet_To_Send : Graph.Packet := new Graph.Packet_Data (I, H);
         begin
            Packets.Append (Packet_To_Send);
            G.Nodes (0).My_Vertice.Receive (Packet_To_Send);
         end;
      end loop;

      Printer.Raports (G.Nodes, Packets);
   end;
end Main;
