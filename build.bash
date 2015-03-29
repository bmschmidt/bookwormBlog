cat _site/posts/* | perl -ne 'if (m/images\/([0-9a-f]+).png".*alt="([^"]+)"/) {print "phantomjs js/renderHTML.js $1 $2\n"}' > args.bash
bash args.bash
