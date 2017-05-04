#!/bin/bash

set -e

# if we are a VAMBE
# if [ "$1" = 'VAMBE' ]; then
#   exec /usr/bin/shiny-server.sh
# fi


#!/bin/bash

set -e

#_CFG=/srv/shiny-server/xxx/config.r

# subs definition go here

# usage: key_value_to_cfg key value
#    ie: file_env 'proxy_url' 'xxx.proxy.de'
# (will write the key/value pair proxy_url xxx.proxy.de
# to the shiny app xxx config file
#key_value_to_cfg() {
#        local key="$1"
#        local value="$2"
#        sed -i  's#^config\[\["'$key'"\]\] <-.*$#config\[\["'$key'"\]\] <- '$value'#g' "$3"
#}

# end of subs definitions

# if we want to start the app
if [ "$1" = 'start-app' ]; then
  if [ "$websockets_behind_proxy" ]; then
     echo "[CUSTOM] setting enhanced websocket settings (useful for problematic corporate proxy servers)"
     # make changes to the app config, this is needed for some problematic
     # cooperate proxy servers
     sed -i.bak.proxy 's#location / {#location / {\napp_init_timeout 18000;\napp_idle_timeout 18000;\ndisable_protocols  xhr-streaming xhr-polling xdr-polling iframe-xhr-polling jsonp-polling;\n#g' /etc/shiny-server/shiny-server.conf 
  fi  
  if [ "$verbose_logfiles" ]; then
     echo "[CUSTOM] setting verbose shiny server and application logfiles"
     # this is a very useful setting for debugging your shiny application..this keeps all logfiles in /var/log/shiny-server. 
     # otherwise log files get deleted if the app crashes[CUSTOM] which is usually not what we want when debugging
     # but dont use this option on production server[CUSTOM] this will fill up space easily
     sed -i.bak.verbose 's#run_as shiny;#run_as shiny;\npreserve_logs true; #g' /etc/shiny-server/shiny-server.conf
     # enable full debugging output for the shiny server
     sed -i.bak.verbose 's#exec shiny-server\(.*\)#export SHINY_LOG_LEVEL=TRACE\nexec shiny-server \1#g' /usr/bin/shiny-server.sh
  fi 
  # if [ "$CUSTOM_KEY" ]; then
#     echo "[CUSTOM] setting the path to the CUSTOM_KEY"  
#     key_value_to_cfg "BLABLABLA" "\"$CUSTOM_KEY\"" "$_CFG"
#   fi
#   if [ "$ANOTHER_CUSTOM_KEY" ]; then
#     echo "[CUSTOM] JUST BLA"
#     key_value_to_cfg "BLABLA2" "\"$ANOTHER_CUSTOM_KEY\"" "$_CFG"
#   fi
  exec /usr/bin/shiny-server.sh
fi
