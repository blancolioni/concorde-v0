import React from 'react';
import { connect } from 'react-redux'
import DashboardCell from './DashboardCell';

class Dashboard extends React.Component {

    render() {
        console.log('dashboard', this.props.boxes);
        return (
            <div className="concorde-dashboard-grid">
                {
                    this.props.boxes[0].mapLeaves(id => this.props.boxes[id], (box) => {
                        console.log("leaf", box);
                        return (
                            <DashboardCell 
                                key={box.id} 
                                boxId={box.id}
                            />
                            );
                        })
                }
            </div>
        );
    }
}

function mapStateToProps(state) {
    return {
      boxes: state.boxes.boxes
    };
  }
export default connect(
    mapStateToProps,
    null
)(Dashboard)
