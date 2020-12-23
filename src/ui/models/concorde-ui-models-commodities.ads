with Nazar.Models.Table;

with Concorde.Handles.Commodity;

package Concorde.UI.Models.Commodities is

   function Commodity_Market_Model
     (Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Nazar.Models.Table.Nazar_Table_Model;

end Concorde.UI.Models.Commodities;
