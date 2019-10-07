import React from 'react';
import { Shell } from './Models/Shell';
import { Table } from './Models/Table';
import { userService } from '../_services';

class DashboardTitleBar extends React.Component {

    render() {
        return (
                <div className="concorde-dashboard-titlebar">
                    <span>{this.props.text} - {localStorage.getItem('user')} - {this.props.clientId}</span>
                    <span className="concorde-titlebar-right">
                        <button onClick={(e) => this.props.onDashboardCommand('splitHorizontal',e)}>
                            <i class="fas fa-grip-lines-vertical"></i>
                        </button>
                        <button onClick={(e) => this.props.onDashboardCommand('splitVertical',e)}>
                            <i class="fas fa-grip-lines"></i>
                        </button>
                        <i class="fas fa-window-close"></i>
                    </span>
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
                    <DashboardTitleBar text={this.props.title} clientId={this.state.clientId} onDashboardCommand={this.props.onDashboardCommand}></DashboardTitleBar>
                    <div className="concorde-dashboard-body">
                        {this.props.children}
                    </div>
                </div>
            );
        }
    }
}

class DashboardCell extends React.Component {

    constructor(props) {
        super(props)
        this.state = {
            anchor: this.props.anchor,
        }
        this.splitHorizontal = this.splitHorizontal.bind(this);
        this.splitVertical = this.splitVertical.bind(this);
        this.onDashboardCommand = this.onDashboardCommand.bind(this);
        this.commands = {
            splitHorizontal: this.splitHorizontal,
            splitVertical: this.splitVertical,
        }
    }

    splitHorizontal() {
        const anchor = this.state.anchor;
        const newCellAnchor = {
            ...anchor,
            left: anchor.left + (anchor.right - anchor.left) / 2,
        }
        this.setState((state) => {
            return { ...state,
               anchor: {
                    ...anchor,
                    right: newCellAnchor.left,
                   },
             }
        });
        this.props.newCell(newCellAnchor);

    }

    splitVertical() {
        const anchor = this.state.anchor;
        const newCellAnchor = {
            ...anchor,
            top: anchor.top + (anchor.bottom - anchor.top) / 2,
        }
        this.setState((state) => {
            return { ...state,
               anchor: {
                    ...anchor,
                    bottom: newCellAnchor.top,
                   },
             }
        });
        this.props.newCell(newCellAnchor);

    }
    onDashboardCommand(cmd,evt) {
        this.commands[cmd]();
    }

    render() {
        let cellStyle = {
            gridColumnStart: this.state.anchor.left,
            gridColumnEnd: this.state.anchor.right,
            gridRowStart: this.state.anchor.top,
            gridRowEnd: this.state.anchor.bottom,
        }
        return (
            <div className="concorde-dashboard-cell" style={cellStyle}>
                <Shell onDashboardCommand={this.onDashboardCommand}></Shell>
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
                        right: 13,
                        bottom: 13,
                    },
                    client: 0,
                },
            ],
        }
        this.newCell = this.newCell.bind(this);
    }

    newCell(anchor) {
        this.setState((state) => {
            return { ...state,
                layout: [
                    ...state.layout, {
                        anchor: anchor,
                        client: 0,
                    }
                ]}
            });
    }

    render() {
        return (
            <div className="concorde-dashboard-grid">
                {this.state.layout.map((cell) => {
                    return (
                        <DashboardCell anchor={cell.anchor} client={cell.client} newCell={this.newCell}></DashboardCell>
                    );
                })}
            </div>
        );
    }
}

export { Dashboard, DashboardItem };