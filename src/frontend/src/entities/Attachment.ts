import { apiClient } from '~/shared/apiClient'

export interface Attachment {
  code: string
  originalNme: string
  title: string
}

export const AttachmentsApi = {
  async getByIds(fileKeys: string[]) {
    const result = await apiClient.post<Attachment[]>('/File/GetMinIoData', { fileKeys })
    return result.data
  },
}
