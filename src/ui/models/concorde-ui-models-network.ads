with Nazar.Models.Numeric;

with Concorde.Db;

package Concorde.UI.Models.Network is

   function Network_Node_Model
     (Network : Concorde.Db.Network_Reference;
      Tag     : String)
     return Nazar.Models.Numeric.Nazar_Float_Model;

end Concorde.UI.Models.Network;
