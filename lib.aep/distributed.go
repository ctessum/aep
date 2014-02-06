package aep

import (
	"bitbucket.org/ctessum/aep/spatialsrg"
	"net"
	"net/http"
	"net/rpc"
)

var RPCport = "6061" // Port for RPC communications for distributed computing

// Set up a server to remotely calculate gridding surrogates.
func DistributedServer(c *RunData) {
	spatialsrg.DebugLevel = c.DebugLevel
	srgGenWorker, err := spatialsrg.NewSrgGenWorker(c)
	if err != nil {
		panic(err)
	}
	rpc.Register(srgGenWorker)
	rpc.HandleHTTP()
	l, err := net.Listen("tcp", ":"+RPCport)
	if err != nil {
		panic(err)
	}
	http.Serve(l, nil)
}
