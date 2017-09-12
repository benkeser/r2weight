#! /bin/bash

# scp files to snail
cd ~/Dropbox/R/r2weight/tests/simulation/
scp cent* sce* dbenkese@snail.fhcrc.org:~/cvr2 

# ssh into snail
ssh dbenkese@snail.fhcrc.org

# scp files to rhino
cd ~/cvr2 
scp cent* sce* dbenkese@rhino.fhcrc.org:~/cvr2 

# ssh into rhino
ssh dbenkese@rhino.fhcrc.org
cd ~/cvr2 

# execute calls
./sce.sh ./cent.R submit5

# and excluding variables
./sce2.sh ./cent2.R submitEx7

# check jobs
squeue -u dbenkese

# get files back
# -- from snail
scp dbenkese@rhino.fhcrc.org:~/cvr2/out/allOut* ~/cvr2
# -- from term
cd /Users/benkeser/Dropbox/R/r2weight/tests/simulation
scp dbenkese@snail.fhcrc.org:~/cvr2/allOut* . 