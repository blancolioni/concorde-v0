import React from 'react';
import { userService } from '../../../_services';
import { DashboardItem } from '../..';

import '../../../Concorde.css';

class Table extends React.Component {

    constructor(props) {
        super(props);

        this.state = {
            tableName: 'Oefeh I Prices',
            tableModel: "market-price",
            tableArg: "Oefeh I",
            tableData: [],
            headings: [],
            data: [],
            clientId: 0,
        }

        this.onConnected = this.onConnected.bind(this);
    }

    onConnected(clientId) {

        userService.postRequest('client/' + clientId, {data: 'get'})
            .then((result) => result.json())   
            .then((resp) => {
                this.setState(state => {
                    return {
                        ...state,
                        clientId: clientId,
                        headings: resp.table.headings,
                        data: resp.table.data,
                    }                    
                });
            });

    }

    render() {
        return (
            <DashboardItem title={this.state.tableName} model={this.state.tableModel} modelArg={this.state.tableArg} onConnected={this.onConnected} >
                <table className="table-sm">
                    <thead>
                        <tr>
                            {this.state.headings.map((heading) => {
                                return <th>{heading}</th>
                                })
                            }
                        </tr>
                    </thead>
                    <tbody>
                        {this.state.data.map((row) => {
                            return (<tr>
                                {row.map((cell) => {
                                    return (<td>{cell}</td>);
                                })}
                            </tr>);
                        })}
                    </tbody>
                </table>
            </DashboardItem>
        );
    }
}

export { Table };
