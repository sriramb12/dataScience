function tarZ()
{
   echo listing $1
   sudo ls -ltr /var/log/contrail/$1
   echo tarball
   sudo tar zcf `date`-$1.tgz /var/log/contrail/$1
}
function Ls()
{
   sudo ls -ltr /var/log/contrail/$1
}

function lastLogs()
{
   sudo find . -mmin -$1 /var/log/ 
}
