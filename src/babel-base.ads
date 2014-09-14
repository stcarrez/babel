-----------------------------------------------------------------------
--  babel-base -- Database for files
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
with Ada.Finalization;
with ADO;

with Babel.Files;
package Babel.Base is

   type Database is abstract new Ada.Finalization.Limited_Controlled with private;

   --  Insert the file in the database.
   procedure Insert (Into : in out Database;
                     File : in Babel.Files.File_Type) is abstract;

private

   type Database is abstract new Ada.Finalization.Limited_Controlled with record
      Name : Integer;
   end record;

end Babel.Base;
