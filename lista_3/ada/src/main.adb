with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Graph;
with Offer;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;
with Random;

procedure Main is
      N       : Positive;
      D       : Natural;
      G       : Graph.Graph;
      Packets : Graph.Packet_Vector.Vector;
      Dummy   : Offer.Offer_Vector.Vector;
begin
      Ada.Text_IO.Put ("Give the number of nodes: ");
      Ada.Integer_Text_IO.Get (N);
      Graph.Global_N := N;
      Ada.Text_IO.Put ("Give the number of shortcuts: ");
      Ada.Integer_Text_IO.Get (D);

      G := Graph.Graph_Generator (N, D);
      Graph.Print_Graph (G);

      Graph.Global_Counter := new Graph.Proper_Counter;

      begin
            for I in 0 .. N - 1 loop
                  G.Nodes (I).My_Receiver :=
                       new Graph.Proper_Receiver (I, G.Nodes (I));
                  Graph.Receivers.Append (G.Nodes (I).My_Receiver);
                  G.Nodes (I).My_Manager :=
                       new Graph.Proper_Manager (I, G.Nodes (I));
                  Graph.Managers.Append (G.Nodes (I).My_Manager);
            end loop;
            for I in 0 .. N - 1 loop
                  G.Nodes (I).My_Sender :=
                       new Graph.Proper_Sender (I, G.Nodes (I));
                  Graph.Senders.Append (G.Nodes (I).My_Sender);
            end loop;
      end;
end Main;
