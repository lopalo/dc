<apply template="base">
    <bind tag="main-content">
        <form action="/ui/kill-process-by-name"
              method="post" class="col-md-8 col-md-offset-2">
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
