with Ada.Text_IO;
with Ada.Containers.Vectors;
with Edge;
with GNAT.Task_Lock;
with Offer;
with State;
with Random;
with Constants;

package body Graph is
   function Graph_Generator (N : Positive; D : Natural) return Graph is
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
   end Print_Graph;

   task body Proper_Sender is
      Inner_List    : Offer.Offer_Vector.Vector;
      Current_Offer : Offer.Offer;
      Successful    : Boolean;
   begin
      delay Random.Rand_Time;
      while Global_Finish = False loop
         Successful := False;
         select
            Managers (I).Sender_Lock;
            Successful := True;
         or
            delay Constants.Timeout;
         end select;
         if Successful then
            accept Sender_Unlock (List : in Offer.Offer_Vector.Vector) do
               Inner_List := List;
            end Sender_Unlock;
            for Idx in Inner_List.First_Index .. Inner_List.Last_Index loop
               Current_Offer := Inner_List (Idx);
               for J in
                 My_Node.Successors.First_Index ..
                   My_Node.Successors.Last_Index
               loop
                  Ada.Text_IO.Put_Line
                    ("SENDER" & Natural'Image (I) & ": Sending offer (" &
                     Natural'Image (Current_Offer.J) & ", " &
                     Natural'Image (Current_Offer.Cost_J) & ")" &
                     " to receiver " & Natural'Image (My_Node.Successors (J)));
                  Receivers (My_Node.Successors (J)).Receiver_Lock
                    (Current_Offer);
               end loop;
            end loop;

            delay Random.Rand_Time;
         end if;
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
                     Global_Counter.Decrement;
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
                  if R (New_Offer.J).Changed = False then
                     Global_Counter.Increment;
                  end if;
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
            terminate;
         end select;
      end loop;
   end Proper_Manager;

   task body Proper_Receiver is
      Inner_Offer : Offer.Offer;
   begin
      loop
         select
            accept Receiver_Lock (New_Offer : Offer.Offer) do
               Inner_Offer := New_Offer;
            end Receiver_Lock;
            Ada.Text_IO.Put_Line
              ("RECEIVER" & Natural'Image (I) & ": Received offer (" &
               Natural'Image (Inner_Offer.J) & ", " &
               Natural'Image (Inner_Offer.Cost_J) & ")");
            Managers (I).Receive_Lock (Inner_Offer);
            accept Receive_Unlock;
         or
            terminate;
         end select;
      end loop;
   end Proper_Receiver;

   task body Proper_Counter is
      Count : Natural := Global_N * (Global_N - 1);
   begin
      loop
         select
            accept Increment do
               Count := Count + 1;
            end Increment;
         or
            accept Decrement do
               Count := Count - 1;
            end Decrement;
         or
            delay Constants.Timeout;
            if Count = 0 then
               delay Constants.Timeout;
               if Count = 0 then
                  Global_Finish := True;
                  exit;
               end if;
            end if;
         end select;
      end loop;
   end Proper_Counter;
end Graph;
