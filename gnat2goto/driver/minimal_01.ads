with Einfo;                 use Einfo;
with Namet;                 use Namet;
with Nlists;                use Nlists;
with Sem_Util;              use Sem_Util;
with Sem_Aux;               use Sem_Aux;
with Snames;                use Snames;
with Stringt;               use Stringt;
with Treepr;                use Treepr;
with Uintp;                 use Uintp;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Follow;                use Follow;
with GNAT_Utils;            use GNAT_Utils;
with GOTO_Utils;            use GOTO_Utils;
with Uint_To_Binary;        use Uint_To_Binary;
with Stand;
with Binary_To_Hex;         use Binary_To_Hex;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;

with GNAT2GOTO.Options;
with Urealp; use Urealp;

with Range_Check; use Range_Check;


with Ada.Text_IO;           use Ada.Text_IO;

with Sem_Util;              use Sem_Util;
with Stand;                 use Stand;
with Switch;                use Switch;
with Einfo;                 use Einfo;
with Atree;                 use Atree;
with Uintp;                 use Uintp;

with Follow;                use Follow;
with Ireps;                 use Ireps;
with Symbol_Table_Info;     use Symbol_Table_Info;

with Tree_Walk;             use Tree_Walk;
with Gather_Irep_Symbols;

with GNATCOLL.JSON;         use GNATCOLL.JSON;

with Sinfo;                 use Sinfo;
with Namet;                 use Namet;
with Lib;                   use Lib;
with GNAT_Utils;            use GNAT_Utils;
with GNAT2GOTO.Options;

package Minimal_01 is

   procedure SayHello;
   procedure Test01;
   
end Minimal_01;
