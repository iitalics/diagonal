roundor_url=https://ff.static.1001fonts.net/r/o/roundornoncommercial.regular.otf
nunito_url=https://www.1001freefonts.com/d/5900/nunito.zip

wget --no-use-server-timestamps "$roundor_url" -O roundor.otf
wget --no-use-server-timestamps "$nunito_url" -O nunito.zip
unzip nunito.zip Nunito-Regular.ttf
mv Nunito-Regular.ttf nunito.ttf
rm nunito.zip
