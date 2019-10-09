import React from 'react';

import { connect } from 'react-redux'
import { splitVertical } from '../redux/actions/boxes';

function DashboardTitleBar(props) {

    return (
        <div className="concorde-dashboard-titlebar">
            <span>{props.text} - {localStorage.getItem('user')} - {props.clientId}</span>
            <span className="concorde-titlebar-right">
                <button className="concorde-titlebar-button" onClick={(e) => props.onDashboardCommand('splitHorizontal',e)}>
                    <i className="fas fa-grip-lines-vertical"></i>
                </button>
                <button className="concorde-titlebar-button" onClick={() => props.splitVertical(props.boxId)}>
                    <i className="fas fa-grip-lines"></i>
                </button>
                <button className="concorde-titlebar-button" onClick={(e) => props.onDashboardCommand('closeDashboardItem',e)}>
                    <i className="fas fa-window-close"></i>
                </button>
            </span>
        </div>
    );
}

export default connect(
    null,
    { splitVertical }
)(DashboardTitleBar)
