with Ada.Text_IO;
with Ada.Containers.Vectors;
with Edge;
with Random;
with Constants;

package body Graph is
   function Graph_Generator (N : Positive; D : Natural) return Graph is
      G                : Graph;
      Shortcuts        : Natural;
      Index            : Natural;
      Edge_To_Add      : Edge.Int_Edge;
      Additional_Edges : Edge.Edge_Vector.Vector;
   begin
      for ID in 0 .. N - 1 loop
         G.Nodes.Append (new Node_Data (ID));
      end loop;

      for ID in 0 .. N - 2 loop
         G.Nodes (ID).Successors.Append (G.Nodes (ID + 1));
      end loop;

      for A in 0 .. N - 3 loop
         for B in A + 2 .. N - 1 loop
            Additional_Edges.Append (new Edge.Int_Edge_Data (A, B));
         end loop;
      end loop;

      if D < (N**2 - N) / 2 - (N - 1) then
         Shortcuts := D;
      else
         Shortcuts := (N**2 - N) / 2 - (N - 1);
      end if;

      while Shortcuts > 0 loop
         Index :=
           Random.Rand_Range
             (Natural (Additional_Edges.First_Index),
              Natural (Additional_Edges.Last_Index));
         Edge_To_Add := Additional_Edges (Index);
         G.Nodes (Edge_To_Add.From).Successors.Append
           (G.Nodes (Edge_To_Add.To));
         Additional_Edges.Delete (Index);
         Shortcuts := Shortcuts - 1;
      end loop;

      return G;
   end Graph_Generator;

   procedure Print_Graph (G : in Graph) is
   begin
      Ada.Text_IO.Put_Line ("GRAPH:");
      for From of G.Nodes loop
         for To of From.Successors loop
            Ada.Text_IO.Put
              (Natural'Image (From.ID) & " ->" & Natural'Image (To.ID));
            Ada.Text_IO.New_Line;
         end loop;
      end loop;
   end Print_Graph;

   task body Proper_Hunter is
      Nodes : Node_Vector.Vector;
   begin
      loop
         delay Random.Rand_Time;
         null;
      end loop;
   end Proper_Hunter;

   task body Proper_Vertice is
      Inner_Packet   : Packet;
      Printer_Server : Printer := My_Printer;
      Next           : Node;
   begin
      loop
         delay Random.Rand_Time;
         select
            accept Receive (Pack : Packet) do
               Inner_Packet := Pack;
            end Receive;
            Inner_Packet.Visited_Nodes.Append (My_Node.ID);
            My_Node.Packets_Passed.Append (Inner_Packet.ID);
            if My_Node.Successors.Is_Empty then
               Printer_Server.Print
                 ("PACKET" & Natural'Image (Inner_Packet.ID) & " RECEIVED",
                  True);
            else
               Printer_Server.Print
                 ("PACKET" & Natural'Image (Inner_Packet.ID) & " IS IN NODE" &
                  Natural'Image (My_Node.ID),
                  False);
               delay Random.Rand_Time;
               Next :=
                 My_Node.Successors
                   (Random.Rand_Range
                      (Natural (My_Node.Successors.First_Index),
                       Natural (My_Node.Successors.Last_Index)));
               Next.My_Vertice.Receive (Inner_Packet);
            end if;
         or
            accept SetTrap (Set : Boolean) do
               Printer_Server.Print ("pulapka tutaj!", False);
            end SetTrap;
         or
            terminate;
         end select;
      end loop;
   end Proper_Vertice;

   task body Proper_Printer is
      Counter : Natural := 0;
   begin
      loop
         select
            accept Print (Text : String; Is_Received : Boolean) do
               Ada.Text_IO.Put_Line (Text);
               if Is_Received then
                  Counter := Counter + 1;
               end if;
            end Print;
         or when Counter = K =>
            accept Raports
              (Nodes : Node_Vector.Vector; Packs : Packet_Vector.Vector)
            do
               Ada.Text_IO.Put_Line ("NODES:");
               for X of Nodes loop
                  Ada.Text_IO.Put (Natural'Image (X.ID) & ":");
                  for Y of X.Packets_Passed loop
                     Ada.Text_IO.Put (Natural'Image (Y));
                  end loop;
                  Ada.Text_IO.New_Line;
               end loop;
               Ada.Text_IO.Put_Line ("PACKETS:");
               for X of Packs loop
                  Ada.Text_IO.Put (Natural'Image (X.ID) & ":");
                  for Y of X.Visited_Nodes loop
                     Ada.Text_IO.Put (Natural'Image (Y));
                  end loop;
                  Ada.Text_IO.New_Line;
               end loop;
            end Raports;
         or
            terminate;
         end select;
      end loop;
   end Proper_Printer;
end Graph;
