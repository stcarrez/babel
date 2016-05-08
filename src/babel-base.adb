-----------------------------------------------------------------------
--  babel-base -- Database for files
--  Copyright (C) 2016 Stephane.Carrez
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

package body Babel.Base is

   procedure Copy (Into : in out Database'Class;
                   From : in Database'Class) is

      procedure Process (File : in Babel.Files.File_Type) is
      begin
         Into.Insert (File);
      end Process;

   begin
      From.Iterate (Process'Access);
   end Copy;

end Babel.Base;
