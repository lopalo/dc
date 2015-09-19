<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">

        <title>DC Admin: <title-text/></title>
        <link href="/static/css/vendors/bootstrap.min.css" rel="stylesheet">
    </head>
    <nav class="navbar navbar-inverse navbar-static-top">
        <div class="container">
            <div class="navbar-header">
                <a class="navbar-brand" href="/ui">DC Admin</a>
            </div>
            <div id="navbar" class="collapse navbar-collapse">
                <ul class="nav navbar-nav">
                    <li class="${registry-nav-class}">
                        <a href="/ui/registry">Registry</a>
                    </li>
                    <li class="${node-statistic-nav-class}">
                        <a href="/ui/node-statistic">Node Statistic</a>
                    </li>
                </ul>
            </div>
        </div>
    </nav>
    <body>
        <div class="container">
            <main-content/>
        </div>
    </body>
</html>
