package aep

import (
	"bitbucket.org/ctessum/gis"
	"net"
	"net/http"
	"net/rpc"
)

var RPCport = "6061" // Port for RPC communications for distributed computing

// Set up a server to remotely calculate gridding surrogates.
func DistributedServer(PGc gis.PostGISconnecter) {
	srgsInGridCell, err := gis.NewSrgsInGridCellCalculator(PGc)
	if err != nil {
		panic(err)
	}
	srgGenWorker, err := gis.NewSrgGenWorker(PGc)
	if err != nil {
		panic(err)
	}
	rpc.Register(srgsInGridCell)
	rpc.Register(srgGenWorker)
	rpc.HandleHTTP()
	l, err := net.Listen("tcp", ":"+RPCport)
	if err != nil {
		panic(err)
	}
	http.Serve(l, nil)
}
