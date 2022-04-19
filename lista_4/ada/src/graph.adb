with Ada.Text_IO;
with Ada.Containers.Vectors;
with Edge;
with GNAT.Task_Lock;
with Offer;
with State;
with Random;
with Ada.Strings.Unbounded;
with Constants;

package body Graph is
   function Graph_Generator
     (N : Positive; D : Natural; H : Natural) return Graph
   is
      G                       : Graph;
      Shortcuts               : Natural;
      Index                   : Natural;
      Edge_To_Add             : Edge.Int_Edge;
      Additional_Normal_Edges : Edge.Edge_Vector.Vector;
   begin
      for ID in 0 .. N - 1 loop
         G.Nodes.Append (new Node_Data (ID));
      end loop;

      for ID in 0 .. N - 2 loop
         G.Nodes (ID).Successors.Append (ID + 1);
      end loop;

      for ID in 1 .. N - 1 loop
         G.Nodes (ID).Successors.Append (ID - 1);
      end loop;

      for A in 0 .. N - 3 loop
         for C in A + 2 .. N - 1 loop
            Additional_Normal_Edges.Append (new Edge.Int_Edge_Data (A, C));
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
             (Natural (Additional_Normal_Edges.First_Index),
              Natural (Additional_Normal_Edges.Last_Index));
         Edge_To_Add := Additional_Normal_Edges (Index);
         G.Nodes (Edge_To_Add.From).Successors.Append (Edge_To_Add.To);
         G.Nodes (Edge_To_Add.To).Successors.Append (Edge_To_Add.From);
         Additional_Normal_Edges.Delete (Index);
         Shortcuts := Shortcuts - 1;
      end loop;

      for I in 1 .. H loop
         Index                           := Random.Rand_Range (0, N - 1);
         G.Nodes (Index).Number_Of_Hosts :=
           G.Nodes (Index).Number_Of_Hosts + 1;
      end loop;

      return G;
   end Graph_Generator;

   procedure Print_Graph (G : in Graph) is
   begin
      Ada.Text_IO.Put_Line ("GRAPH:");
      for From of G.Nodes loop
         for To of From.Successors loop
            Ada.Text_IO.Put
              (Natural'Image (From.ID) & " ->" & Natural'Image (To));
            Ada.Text_IO.New_Line;
         end loop;
      end loop;
      Ada.Text_IO.Put_Line ("HOSTS:");
      for Node of G.Nodes loop
         Ada.Text_IO.Put_Line
           (Natural'Image (Node.ID) & " has" &
            Natural'Image (Node.Number_Of_Hosts) & " hosts");
      end loop;
   end Print_Graph;

   task body Proper_Sender is
      Inner_List    : Offer.Offer_Vector.Vector;
      Current_Offer : Offer.Offer;
   begin
      loop
         delay 2 * Random.Rand_Time;
         Managers (I).Sender_Lock;
         accept Sender_Unlock (List : in Offer.Offer_Vector.Vector) do
            Inner_List := List;
         end Sender_Unlock;
         for Idx in Inner_List.First_Index .. Inner_List.Last_Index loop
            Current_Offer := Inner_List (Idx);
            for J in
              My_Node.Successors.First_Index .. My_Node.Successors.Last_Index
            loop
               Receivers (My_Node.Successors (J)).Receiver_Lock
                 (Current_Offer);
            end loop;
         end loop;
      end loop;
   end Proper_Sender;

   task body Proper_Manager is
      R        : State.State_Vector.Vector;
      List     : Offer.Offer_Vector.Vector;
      New_Cost : Natural;

   begin
      for J in 0 .. I - 1 loop
         R.Append (new State.State_Data (True, I - 1, I - J));
      end loop;
      R.Append (new State.State_Data (False, I, 0));
      for J in I + 1 .. Global_N - 1 loop
         R.Append (new State.State_Data (True, I + 1, J - I));
      end loop;
      for J in My_Node.Successors.First_Index .. My_Node.Successors.Last_Index
      loop
         R (My_Node.Successors (J)) :=
           new State.State_Data (True, My_Node.Successors (J), 1);
      end loop;
      loop
         select
            accept Sender_Lock do
               List.Clear;
               for K in 0 .. Global_N - 1 loop
                  if R (K).Changed then
                     R (K) :=
                       new State.State_Data (False, R (K).Nexthop, R (K).Cost);
                     Ada.Text_IO.Put_Line
                       ("CHANGE: R_" & Natural'Image (I) & "[" &
                        Natural'Image (K) & "] = " & "{changed: false, " &
                        "nexthop: " & Natural'Image (R (K).Nexthop) &
                        ", cost: " & Natural'Image (R (K).Cost) & "}");
                     List.Append (new Offer.Offer_Data (I, K, R (K).Cost));
                  end if;
               end loop;
            end Sender_Lock;
            Senders (I).Sender_Unlock (List);
         or
            accept Receive_Lock (New_Offer : Offer.Offer) do
               New_Cost := New_Offer.Cost_J + 1;
               if R (New_Offer.J).Cost > New_Cost then
                  R (New_Offer.J) :=
                    new State.State_Data (True, New_Offer.L, New_Cost);
                  Ada.Text_IO.Put_Line
                    ("CHANGE: R_" & Natural'Image (I) & "[" &
                     Natural'Image (New_Offer.J) & "] = " &
                     "{changed: true, " & "nexthop: " &
                     Natural'Image (New_Offer.L) & ", cost: " &
                     Natural'Image (New_Cost) & "}");
               end if;
            end Receive_Lock;
            Receivers (I).Receive_Unlock;
         or
            accept Forwarder_Lock (Idx : in Natural; Next_Hop : out Natural) do
               Next_Hop := R (Idx).Nexthop;
            end Forwarder_Lock;
         end select;
      end loop;
   end Proper_Manager;

   task body Proper_Receiver is
      Inner_Offer : Offer.Offer;
   begin
      loop
         accept Receiver_Lock (New_Offer : Offer.Offer) do
            Inner_Offer := New_Offer;
         end Receiver_Lock;
         Managers (I).Receive_Lock (Inner_Offer);
         accept Receive_Unlock;
      end loop;
   end Proper_Receiver;

   task body Proper_Forwarder_Sender is
      Packet   : Standard_Packet;
      Next_Hop : Natural;
   begin
      loop
         My_Node.My_Forwarder_Queue.Dequeue (Packet);
         Packet.Visited_Routers :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Ada.Strings.Unbounded.To_String (Packet.Visited_Routers) &
              Natural'Image (R));
         if R = Packet.RD then
            My_Node.My_Hosts (Packet.HD).Host_Get (Packet);
         else
            My_Node.My_Manager.Forwarder_Lock (Packet.RD, Next_Hop);
            Forwarder_Receivers (Next_Hop).Forwarder_Get (Packet);
         end if;
      end loop;
   end Proper_Forwarder_Sender;

   task body Proper_Forwarder_Receiver is
      Inner_Packet : Standard_Packet;
   begin
      loop
         accept Forwarder_Get (Packet : Standard_Packet) do
            Inner_Packet := Packet;
         end Forwarder_Get;
         My_Node.My_Forwarder_Queue.Enqueue (Inner_Packet);
      end loop;
   end Proper_Forwarder_Receiver;

   task body Proper_Host is
      Inner_Packet : Standard_Packet;
      New_Packet   : Standard_Packet;
   begin
      accept Run (Init_R : in Natural; Init_H : in Natural) do
         Inner_Packet := new Standard_Packet_Data (R, H, Init_R, Init_H);
      end Run;
      My_Node.My_Forwarder_Receiver.Forwarder_Get (Inner_Packet);
      delay Random.Rand_Time;
      loop
         accept Host_Get (Packet : in Standard_Packet) do
            Inner_Packet := Packet;
         end Host_Get;
         Ada.Text_IO.Put_Line
           ("PACKET: from(" & Natural'Image (Inner_Packet.RS) & "," &
            Natural'Image (Inner_Packet.HS) & ") to (" &
            Natural'Image (Inner_Packet.RD) & "," &
            Natural'Image (Inner_Packet.HS) & "), routers:" &
            Ada.Strings.Unbounded.To_String (Inner_Packet.Visited_Routers));
         delay Random.Rand_Time;
         New_Packet :=
           new Standard_Packet_Data (R, H, Inner_Packet.RS, Inner_Packet.HS);
         My_Node.My_Forwarder_Receiver.Forwarder_Get (New_Packet);
      end loop;
   end Proper_Host;
end Graph;
