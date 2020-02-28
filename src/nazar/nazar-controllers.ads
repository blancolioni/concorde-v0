package Nazar.Controllers is

   type Controller_Interface is interface;

   subtype Controller_Class is Controller_Interface'Class;

   type Controller_Type is access all Controller_Interface'Class;

end Nazar.Controllers;
