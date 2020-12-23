with Concorde.Handles.Colony;
with Concorde.Handles.Policy;
with Concorde.Handles.Pop;

package Concorde.Colonies is

   procedure Execute_Daily_Policy
     (Colony  : Concorde.Handles.Colony.Colony_Class;
      Policy  : Concorde.Handles.Policy.Policy_Class);

   procedure Daily_Tax_Revenue
     (Colony  : Concorde.Handles.Colony.Colony_Class;
      Pop     : Concorde.Handles.Pop.Pop_Class);

end Concorde.Colonies;
