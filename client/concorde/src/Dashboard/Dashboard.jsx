import React from 'react';
import { Shell } from '../Shell';
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
        
        userService.postRequest('new-client', { model: this.props.model})
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

class Dashboard extends React.Component {
    render() {
        return (
            <div className="container">
                <div className="row">
                    <div className="col-12">
                        <Shell>
                        </Shell>
                    </div>
                </div>
            </div>
        );
    }
}

export { Dashboard, DashboardItem };