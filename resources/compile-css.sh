echo "This duder recompiles the bootstrap.min.css within the generic bootstrap less directory."
echo ""
echo "If you would like to modify any of those .less files, please copy to the css/ root and symlink back from css/bootstrap-x.x.x/less/xxx.less. This way the changes can be tracked in our repository."
echo ""
lessc -x css/bootstrap-3.3.7/less/bootstrap.less > css/bootstrap-3.3.7/less/bootstrap.min.css
