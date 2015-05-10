cat _site/posts/* | perl -ne 'if (m/hashcode="([0-9a-f]+)".*jsonQuery="([^"]+)"/) {print "phantomjs js/renderHTML.js $1 $2\n"}' > args.bash
cat _site/posts/* | perl -ne 'if (m/images\/([0-9a-f]+).png".*alt="([^"]+)"/) {print "wget -nc -O json/$1.json http://localhost/cgi-bin/dbbindings.py?query=$2\n"}' > download.bash

#bash args.bash
