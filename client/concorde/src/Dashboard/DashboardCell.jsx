import React from 'react';
import { connect } from 'react-redux'

import { userService } from '../_services';
import { setClient } from '../redux/actions/boxes';

import { Galaxy } from './Models/Galaxy';
import { Shell } from './Models/Shell';
import { Table } from './Models/Table';

const viewMap = {
    Shell: Shell,
    Table: Table,
    Galaxy: Galaxy,
}

class DashboardCell extends React.Component {

    constructor(props) {
        super(props);
        this.state = {
            loaded: false,
        }
    }

    componentDidMount() {
        if (!this.state.loaded) {
            userService.postRequest('new-client', { model: 'shell', modelArg: ''})
            .then(response => response.json())
            .then(
                result => {
                    this.props.setClient({
                        boxId: this.props.boxId,
                        view: Shell,
                        model: 'shell',
                        modelArgs: '',
                        title: 'Concorde Shell',
                        client: result.clientId,
                    });
                    this.setState({
                        loaded: true,
                    });
                    if (this.props.onConnected) {
                        this.props.onConnected (result.clientId);
                    }
                }
            )
    
        }
    }

    controlHandler(setClient, boxId, packet) {
        for(const cmd of packet) {
            if (cmd.control === 'replace-view') {
                if (viewMap[cmd.view]) {
                    setClient({
                        boxId: boxId,
                        view: viewMap[cmd.view],
                        model: cmd.model,
                        modelArgs: cmd.modelArgs,
                        title: cmd.view,
                        client: cmd.clientId,
                       });
                }
            }
        }
    }

    render() {
        console.log("dashboard-cell", this.props);
        const boxId = this.props.boxId;
        const box = this.props.boxes[boxId];
        const anchor = box.anchor;

        let cellStyle = {
            gridColumnStart: anchor.left,
            gridColumnEnd: anchor.right,
            gridRowStart: anchor.top,
            gridRowEnd: anchor.bottom,
        }

        const component = box.childComponent;

        if (component) {
            const View = component.view;

            return (
                <div className="concorde-dashboard-cell" style={cellStyle}>
                    <View
                    title={component.title}
                    model={component.model}
                    boxId={boxId}
                    modelArgs={component.modelArgs}
                    controlHandler={(packet) => { this.controlHandler(this.props.setClient, boxId, packet)}}
                    />                       
                </div>
            );
        } else {
            return (<div
                       className="concorde-dashboard-cell concorde-empty-cell"
                       style={cellStyle} 
                    >
                        Loading ...
                    </div>
                );
        }
    }
}

function mapStateToProps(state) {
    return {
      boxes: state.boxes.boxes
    };
  }
export default connect(
    mapStateToProps,
    { setClient }
)(DashboardCell)
