
define(function (require) {
    var Registry = require("cluster/registry");
    var StartServiceForm = require("cluster/start-service");
    var NodeStatus = require("cluster/node-status");
    var ClusterMesh = require("cluster/cluster-mesh");

    var AreaStatus = require("area/status");

    var pages = [
        {
            section: "cluster",
            title: "Cluster",
            pages: [
                {
                    id: "registry",
                    title: "Registry",
                    component: Registry
                },
                {
                    id: "start-service",
                    title: "Start Service",
                    component: StartServiceForm
                },
                {
                    id: "node-status",
                    title: "Node Status",
                    component: NodeStatus
                },
                {
                    id: "mesh",
                    title: "Cluster Mesh",
                    component: ClusterMesh
                }
            ]
        },
        {
            section: "area",
            title: "Area",
            pages: [
                {
                    id: "status",
                    title: "Area Status",
                    component: AreaStatus
                },
            ]
        }
    ];
    return pages;
});

