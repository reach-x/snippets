<?php

$sample_email = <<<EOS
Vehicle Information

Vehicle Year: 2014
Vehicle Make: Audi
Vehicle Model: A8
Vehicle Trim: 4.0T quattro 4dr Sedan AWD (4.0L 8cyl Turbo 8A)
Mileage: 37000
VIN: %%CAR_VIN%%
No accidents/no rental or fleet/no modifications: on

Contact Information

First Name: John
Last Name: Smith
Phone Number: 3105555555
Email Address: Johnsmith@superemailaccounts.com
Zip Code: 90717
City: Lomita
State: CA
EOS;


$sample_email = preg_replace("/(\%\%.*\%\%)/","",$sample_email);



printf("%s\n",$sample_email);
