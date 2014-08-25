-----------------------------------------------------------------------
--  babel-stores -- Storage management
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
with Babel.Files;
with Babel.Files.Buffers;
with Babel.Filters;
package Babel.Stores is

   type Store_Type is limited interface;
   type Store_Type_Access is access all Store_Type'Class;

   procedure Read (Store : in out Store_Type;
                   Path  : in String;
                   Into  : in out Babel.Files.Buffers.Buffer) is abstract;

   procedure Write (Store : in out Store_Type;
                    Path  : in String;
                    Into  : in Babel.Files.Buffers.Buffer) is abstract;

   procedure Scan (Store  : in out Store_Type;
                   Path   : in String;
                   Into   : in out Babel.Files.File_Container'Class;
                   Filter : in Babel.Filters.Filter_Type'Class) is abstract;

end Babel.Stores;
