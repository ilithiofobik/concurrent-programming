with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Graph;
with Offer;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;
with Random;

procedure Main is
   N        : Positive;
   D        : Natural;
   H        : Natural;
   G        : Graph.Graph;
   Packets  : Graph.Packet_Vector.Vector;
   Dummy    : Offer.Offer_Vector.Vector;
   Random_R : Natural;
   Random_H : Natural;
begin
   Ada.Text_IO.Put ("Give the number of nodes: ");
   Ada.Integer_Text_IO.Get (N);
   Graph.Global_N := N;
   Ada.Text_IO.Put ("Give the number of shortcuts: ");
   Ada.Integer_Text_IO.Get (D);
   Ada.Text_IO.Put ("Give the number of hosts: ");
   Ada.Integer_Text_IO.Get (H);

   G := Graph.Graph_Generator (N, D, H);
   Graph.Print_Graph (G);

   begin
      for I in 0 .. N - 1 loop
         G.Nodes (I).My_Receiver := new Graph.Proper_Receiver (I, G.Nodes (I));
         Graph.Receivers.Append (G.Nodes (I).My_Receiver);
         G.Nodes (I).My_Manager := new Graph.Proper_Manager (I, G.Nodes (I));
         Graph.Managers.Append (G.Nodes (I).My_Manager);
         G.Nodes (I).My_Forwarder_Receiver :=
           new Graph.Proper_Forwarder_Receiver (I, G.Nodes (I));
         Graph.Forwarder_Receivers.Append (G.Nodes (I).My_Forwarder_Receiver);
         G.Nodes (I).My_Forwarder_Sender :=
           new Graph.Proper_Forwarder_Sender (I, G.Nodes (I));
      end loop;
      for I in 0 .. N - 1 loop
         G.Nodes (I).My_Sender := new Graph.Proper_Sender (I, G.Nodes (I));
         Graph.Senders.Append (G.Nodes (I).My_Sender);
      end loop;
      for I in 0 .. N - 1 loop
         for J in 0 .. G.Nodes (I).Number_Of_Hosts - 1 loop
            G.Nodes (I).My_Hosts.Append
              (new Graph.Proper_Host (I, J, G.Nodes (I)));
         end loop;
      end loop;
      for I in 0 .. N - 1 loop
         for J in 0 .. G.Nodes (I).Number_Of_Hosts - 1 loop
            Random_R := I;
            Random_H := J;
            while Random_R = I and Random_H = J loop
               Random_R := Random.Rand_Range (0, N - 1);
               if 0 < G.Nodes (Random_R).Number_Of_Hosts then
                  Random_H :=
                    Random.Rand_Range
                      (0, G.Nodes (Random_R).Number_Of_Hosts - 1);
               else
                  Random_R := I;
                  Random_H := J;
               end if;
            end loop;
            G.Nodes (I).My_Hosts (J).Run (Random_R, Random_H);
         end loop;
      end loop;
   end;
end Main;
