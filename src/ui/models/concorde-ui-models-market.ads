with Nazar.Models.Table;

with Concorde.Handles.Market;

package Concorde.UI.Models.Market is

   function Market_Model
     (Market : Concorde.Handles.Market.Market_Handle)
      return Nazar.Models.Table.Nazar_Table_Model;

end Concorde.UI.Models.Market;
