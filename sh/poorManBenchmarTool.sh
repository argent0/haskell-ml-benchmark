count=0
while test $count -lt 100
do
   start="$(date +%s.%N)"
   $1 > /dev/null
   end="$(date +%s.%N)"
   
   runtime=$( echo "$end - $start" | bc -l )
   
   printf "%s\n" $runtime
   count=$((count + 1))
done
