# cache_server

### compile
* make
 
### start
* ./_rel/cache_server_release/bin/cache_server_release start

###### requests:
*curl -H "Content-Type: application/json" -X POST -d '{"action":"insert","key":"some_key", "value":[1,2,3],"time":6000}' http://localhost:8080/api/cache_server
* curl -H "Content-Type: application/json" -X POST -d '{"action":"lookup","key":"some_key"}' http://localhost:8080/api/cache_server
* curl -H "Content-Type: application/json" -X POST -d '{"action":"lookup_by_date","date_from":"2018/4/9 00:00:00", "date_to":"2018/4/9 23:59:59"}' http://localhost:8080/api/cache_server

### stop
* ./_rel/cache_server_release/bin/cache_server_release stop