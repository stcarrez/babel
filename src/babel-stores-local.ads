-----------------------------------------------------------------------
--  babel-stores-local -- Store to access local files
--  Copyright (C) 2014 Stephane.Carrez
--  Written by Stephane.Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

package Babel.Stores.Local is

   type Local_Store_Type is new Babel.Stores.Store_Type with private;

   --  Get the absolute path to access the local file.
   function Get_Absolute_Path (Store : in Local_Store_Type;
                               Path  : in String) return String;

   overriding
   procedure Read (Store : in out Local_Store_Type;
                   Path  : in String;
                   Into  : in out Babel.Files.Buffers.Buffer);

   overriding
   procedure Write (Store : in out Local_Store_Type;
                    Path  : in String;
                    Into  : in Babel.Files.Buffers.Buffer);

   overriding
   procedure Scan (Store  : in out Local_Store_Type;
                   Path   : in String;
                   Into   : in out Babel.Files.File_Container'Class;
                   Filter : in Babel.Filters.Filter_Type'Class);

   --  Set the root directory for the local store.
   procedure Set_Root_Directory (Store : in out Local_Store_Type;
                                 Path  : in String);

private

   type Local_Store_Type is new Babel.Stores.Store_Type with record
      Root_Dir : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Babel.Stores.Local;
