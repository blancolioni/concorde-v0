import React from 'react';
import { Shell } from './Models/Shell';
import { Table } from './Models/Table';
import { userService } from '../_services';

class DashboardTitleBar extends React.Component {

    render() {
        return (
                <div className="concorde-dashboard-titlebar">
                    {this.props.text} - {localStorage.getItem('user')} - {this.props.clientId}
                </div>
        );
    }
}

class DashboardItem extends React.Component {

    constructor (props) {
        super(props);

        this.state = {
            loading: true,
        }
    }

    componentDidMount() {
        
        userService.postRequest('new-client', { model: this.props.model, modelArg: this.props.modelArg})
        .then(response => response.json())
        .then(
            result => {
                this.setState({
                    isLoaded: true,
                    clientId: result.clientId,
                });
                if (this.props.onConnected) {
                    this.props.onConnected (result.clientId);
                }
            }
        )
    }
    
    render() {
        if (!this.state.isLoaded) {
            return (
                <div className="concorde-loading">Loading ...</div>
            );
        } else {
            return (
                <div className="concorde-dashboard-item">
                    <DashboardTitleBar text={this.props.title} clientId={this.state.clientId}></DashboardTitleBar>
                    <div className="concorde-dashboard-body">
                        {this.props.children}
                    </div>
                </div>
            );
        }
    }
}

class DashboardCell extends React.Component {

    render() {
        let cellStyle = {
            gridColumnStart: this.props.anchor.left,
            gridColumnEnd: this.props.anchor.right,
            gridRowStart: this.props.anchor.top,
            gridRowEnd: this.props.anchor.bottom,
        }
        return (
            <div className="concorde-dashboard-cell" style={cellStyle}>
                <Table></Table>
            </div>
        );
    }
}

class Dashboard extends React.Component {

    constructor(props) {
        super(props);
        this.state = {
            layout: [
                {
                    anchor: {
                        left: 1,
                        top: 1,
                        right: 7,
                        bottom: 7,
                    },
                    client: 0,
                },
            ],
        }
    }

    render() {
        return (
            <div className="concorde-dashboard-grid">
                {this.state.layout.map((cell) => {
                    return (
                        <DashboardCell anchor={cell.anchor} client={cell.client}></DashboardCell>
                    );
                })}
            </div>
        );
    }
}

export { Dashboard, DashboardItem };