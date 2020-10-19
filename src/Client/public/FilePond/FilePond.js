import React from 'react';
import * as FilePondPluginImageExifOrientation from "./filepond-plugin-image-exif-orientation";

import { FilePond, registerPlugin } from "react-filepond";
import * as FilePondPluginImagePreview from 'filepond-plugin-image-preview';
import FilePondPluginFileValidateType from 'filepond-plugin-file-validate-type';

registerPlugin(FilePondPluginImageExifOrientation);
registerPlugin(FilePondPluginImagePreview);
registerPlugin(FilePondPluginFileValidateType);

export class FilePondComponent extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            keepFilesAfterProcessing: props.keepFilesAfterProcessing,
            files: props.keepFilesAfterProcessing ? props.files : []
        };
    }

    render() {
        return (
            <FilePond
                acceptedFileTypes={this.props.acceptedFileTypes}
                allowMultiple={this.props.allowMultiple}
                maxFiles={this.props.maxFiles}
                disabled={this.props.disabled}
                server={this.props.server}
                chunkUploads={true}
                chunkForce={true}
                chunkSize={this.props.chunkSize}
                onprocessfile={this.props.onProcessFile}
                onremovefile={this.props.onRemoveFile}
                labelIdle={this.props.labelIdle}
                files={this.state.files}
                onupdatefiles={(fileItems) => {
                    if (this.state.keepFilesAfterProcessing) {
                        this.setState({
                            files: fileItems.map(fileItem => fileItem.file)
                        });
                    }
                }}
                ref={ref => this.pond = ref} />
        )
    }
}
