with Nazar.Models.Numeric;

with Concorde.Handles.Network;

package Concorde.UI.Models.Network is

   function Network_Node_Model
     (Network : Concorde.Handles.Network.Network_Class;
      Tag     : String)
     return Nazar.Models.Numeric.Nazar_Float_Model;

end Concorde.UI.Models.Network;
