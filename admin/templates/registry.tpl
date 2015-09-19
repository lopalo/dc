<apply template="base">
    <bind tag="title-text">Registry</bind>
    <bind tag="registry-nav-class">active</bind>
    <!-- TODO: fix it -->
    <bind tag="node-statistic-nav-class"></bind>
    <bind tag="main-content">
        <form action="/ui/kill-process-by-name" method="post">
        <table class="table stripped">
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Node ID</th>
                    <th>Kill </th>
                </tr>
            </thead>
            <tbody>
                <name-records>
                    <tr>
                        <td><name/></td>
                        <td><node-id/></td>
                        <td>
                            <button name="name"
                                    value="${name}"
                                    type="submit"
                                    class="btn btn-default">
                                <span class="glyphicon glyphicon-remove"></span>
                            </button>
                        </td>
                    </tr>
                </name-records>
            </tbody>
        </table>
        </form>
    </bind>
</apply>
